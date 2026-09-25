import java.sql.{Connection, DriverManager, Date}
import java.util.Properties
import java.time.{LocalDate, Period}
import requests._
import ujson._
import org.jsoup.Jsoup
import scala.jdk.CollectionConverters._

// --- DATA MODELS ---
case class PlayerCardData(nombre: String, media: Int, posicion: String, fotoUrl: String, clubUrl: String, flagUrl: String, clubNombre: String, div: Int, han: Int, kic: Int, ref: Int, spd: Int, pos: Int, divRaw: Double, hanRaw: Double, kicRaw: Double, refRaw: Double, spdRaw: Double, posRaw: Double, fechaNacimiento: String, rffmUrl: String, rffmName: String, categoria: String)
// MatchLog completo para Moneyball
case class MatchLog(id: Int, rival: String, resultado: String, minutos: Int, nota: Double, fecha: String, clima: String, estadio: String, notas: String, video: String, reaccion: String, status: String, tipo: String, pcTot: Int, pcOk: Int, plTot: Int, plOk: Int, analisisVoz: String, torneoNombre: String, fase: String, paradas: Int, p1v1: Int, pAir: Int, pPie: Int, zTiros: String, zGoles: String, cpi: Option[Double] = None)
case class SeasonSummary(id: Int, categoria: String, clubUrl: String, fotoUrl: String, partidosJugados: Int, golesContra: Int, porteriasCero: Int, mediaFinal: Int)
case class Achievement(icono: String, nombre: String, cantidad: Int, descripcion: String)
case class GearItem(id: Int, nombre: String, tipo: String, usos: Int, maxUsos: Int, estado: String, img: String)
case class Objective(id: Int, tipo: String, actual: Double, meta: Int, descripcion: String)
case class Drill(id: Int, nombre: String, desc: String, actual: Int, objetivo: Int)
case class VideoTag(id: Int, matchId: Int, minuto: Int, segundo: Int, tipo: String, desc: String)
case class VideoClip(tagId: Int, matchId: Int, rival: String, fecha: String, videoUrl: String, minuto: Int, segundo: Int, tipo: String)
case class RivalInfo(nombre: String, estilo: String, claves: String, notas: String)
case class PenaltyStat(zona: String, total: Int, goles: Int)
case class RPGStatus(nivel: Int, xp: Int, nextLevelXp: Int, titulo: String, cinturonJudo: String)
case class TechReview(id: Int, fecha: String, blocaje: Int, pies: Int, aereo: Int, valentia: Int, concentracion: Int, coordinacion: Int, notas: String)
// Academic notes
case class AcademicNote(id: Int, fecha: String, asignatura: String, nota: Double, tipo: String)
// Vault Medico
case class MedicalReport(id: Int, fecha: String, tipo: String, diagnostico: String, recomendaciones: String, esPrevio: Boolean)
// Checklist de habilidades tecnicas de portero
case class GoalkeeperSkill(
  id: Int, categoria: String, habilidad: String, conseguido: Boolean,
  fechaConseguido: Option[String], contextoConseguido: Option[String], notas: String,
  fechaInicioTrabajo: Option[String] = None,
  // BLOQUE N: nivel de automatismo — NULL/CONSCIENTE/AUTOMATICO/INSTINTIVO
  nivelAutomatismo: Option[String] = None
)
// Registro de visibilidad y oportunidades (torneos, pruebas, contactos, ojeadores...)
case class Opportunity(
  id: Int, fecha: String, tipo: String, descripcion: String, clubOEntidad: String,
  resultado: String, seguimiento: String, seguimientoCompletado: Boolean, contactId: Option[Int] = None
)
// Modulo 7: Red de contactos (mini CRM)
case class Contact(
  id: Int, nombre: String, rol: String, clubOEntidad: String, telefono: String, email: String,
  comoConocido: String, ultimaInteraccion: Option[String], notas: String, importancia: String, createdAt: String
)
// Periodizacion anual del calendario de entrenamiento
case class PeriodizationBlock(
  id: Int, nombre: String, fechaInicio: String, fechaFin: String, tipo: String, notas: String, color: String
)
// Sesion de entrenamiento (usada para el listado de Academia y su audio-diario)
case class TrainingSession(
  id: Int, fecha: String, tipo: String, foco: String, rpe: Int, calidad: Int, atencion: Int,
  analisisVozAcademia: String
)
// Footbar (sensor GPS de rendimiento fisico/tecnico)
case class FootbarSession(
  matchId: Int, distanciaKm: Double, altaIntensidadM: Double, sprintMaxKmh: Double,
  pctActividad: Double, tiempoActividadMin: Int, aceleraciones: Int, desaceleraciones: Int,
  balones: Int, pases: Int, tiempoBalonSeg: Int, disparos: Int, tiroMaxKmh: Double
)

object DatabaseManager {
  private val debugMode = sys.env.getOrElse("DEBUG_MODE", "false") == "true"
  private val dbHost = sys.env.getOrElse("DB_HOST", "")
  private val dbName = sys.env.getOrElse("DB_NAME", "")
  private val dbUser = sys.env.getOrElse("DB_USER", "")
  private val dbPass = sys.env.getOrElse("DB_PASS", "")

  val url = s"jdbc:postgresql://$dbHost/$dbName?sslmode=require&options=-c%20client_encoding=UTF8"

  // --- POOL DE CONEXIONES (HikariCP) ---
  // Se inicializa UNA sola vez al arrancar. Neon free tier soporta ~10 conexiones;
  // con maximumPoolSize=5 dejamos margen para el dashboard de Neon.
  private val pool: com.zaxxer.hikari.HikariDataSource = {
    require(dbHost.nonEmpty, "DB_HOST no configurado")
    if (dbPass.isEmpty) throw new IllegalStateException("DB_PASS no configurada. Anadela como variable de entorno.")
    val config = new com.zaxxer.hikari.HikariConfig()
    config.setJdbcUrl(url)
    config.setUsername(dbUser)
    config.setPassword(dbPass)
    config.setMaximumPoolSize(10)
    config.setMinimumIdle(1)
    config.setConnectionTimeout(10000)   // 10s esperando conexion libre del pool
    config.setIdleTimeout(300000)        // Cierra idle tras 5 min
    config.setMaxLifetime(600000)        // Vida max de conexion: 10 min
    config.setKeepaliveTime(120000)      // Ping cada 2 min para mantener vivas las conexiones
    config.addDataSourceProperty("ssl", "true")
    config.setConnectionInitSql("SET client_encoding TO 'UTF8'")
    new com.zaxxer.hikari.HikariDataSource(config)
  }

  // API identica a antes: todos los metodos del DatabaseManager siguen igual.
  // La unica diferencia es que ahora devuelve una conexion del pool, no una nueva.
  def getConnection(): Connection = pool.getConnection()

  // ══════════════════════════════════════════════════════════════════
  // REGLA PERMANENTE — RESTAS DE FECHAS EN POSTGRESQL
  // NUNCA restar dos columnas DATE o TIMESTAMP directamente.
  // SIEMPRE usar DateUtils (DateUtils.scala):
  //   ${DateUtils.daysBetweenSQL("fecha1", "fecha2")}  /  ${DateUtils.daysFromTodaySQL("fecha")}
  // que generan EXTRACT(EPOCH FROM (...::timestamp - ...::timestamp)) / 86400.
  // La resta directa (fecha1 - fecha2) produce un tipo 'interval' en
  // PostgreSQL que no es compatible con comparaciones numéricas y
  // genera el error: operator does not exist: timestamp - integer
  // ══════════════════════════════════════════════════════════════════

  // --- INICIALIZACION DE TABLAS (se llama UNA vez al arrancar el servidor) ---
  // Centraliza todos los CREATE TABLE IF NOT EXISTS que antes estaban dispersos
  // por cada metodo de consulta, eliminando el overhead en cada request.
  def initDB(): Unit = {
    val conn = getConnection()
    try {
      val stmt = conn.createStatement()

      // Temporadas y partidos (nucleo del sistema)
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS seasons (
        id               SERIAL PRIMARY KEY,
        nombre_club      TEXT,
        categoria        TEXT,
        foto_jugador_url TEXT,
        club_escudo_url  TEXT,
        media            DOUBLE PRECISION DEFAULT 59,
        stat_div         DOUBLE PRECISION DEFAULT 80,
        stat_han         DOUBLE PRECISION DEFAULT 60,
        stat_kic         DOUBLE PRECISION DEFAULT 55,
        stat_ref         DOUBLE PRECISION DEFAULT 60,
        stat_spd         DOUBLE PRECISION DEFAULT 62,
        stat_pos         DOUBLE PRECISION DEFAULT 58,
        fecha_inicio     DATE,
        fecha_nacimiento DATE DEFAULT '2020-06-19',
        rffm_url         TEXT,
        rffm_team_name   TEXT,
        judo_belt        TEXT DEFAULT 'Blanco'
      )""")
      // BLOQUE A1 (RFMF): liga oficial de la temporada activa vs interna/amistosa
      stmt.executeUpdate("ALTER TABLE seasons ADD COLUMN IF NOT EXISTS rffm_nombre_equipo TEXT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE seasons ADD COLUMN IF NOT EXISTS rffm_grupo_id TEXT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE seasons ADD COLUMN IF NOT EXISTS liga_tipo TEXT DEFAULT 'INTERNA'")

      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS matches (
        id               SERIAL PRIMARY KEY,
        season_id        INT,
        fecha            DATE,
        rival            TEXT,
        tipo_partido     TEXT DEFAULT 'LIGA',
        status           TEXT DEFAULT 'SCHEDULED',
        goles_favor      INT DEFAULT 0,
        goles_contra     INT DEFAULT 0,
        minutos          INT DEFAULT 0,
        nota             DOUBLE PRECISION DEFAULT 0,
        paradas          INT DEFAULT 0,
        paradas_1v1      INT DEFAULT 0,
        paradas_aereas   INT DEFAULT 0,
        acciones_pie     INT DEFAULT 0,
        clima            TEXT,
        estadio          TEXT,
        temperatura      INT,
        notas_partido    TEXT,
        video_url        TEXT,
        reaccion_goles   TEXT,
        analisis_voz     TEXT,
        zona_goles       TEXT,
        zona_paradas     TEXT,
        zona_tiros       TEXT,
        mapa_campo       TEXT,
        pc_t             INT DEFAULT 0,
        pc_ok            INT DEFAULT 0,
        pl_t             INT DEFAULT 0,
        pl_ok            INT DEFAULT 0,
        media_historica  DOUBLE PRECISION DEFAULT 0,
        torneo_nombre    TEXT,
        fase             TEXT
      )""")

      // Biometria y salud
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS wellness (
        id            SERIAL PRIMARY KEY,
        fecha         DATE DEFAULT CURRENT_DATE,
        sueno         INT,
        horas_sueno   DOUBLE PRECISION,
        energia       INT,
        dolor         INT,
        zona_dolor    TEXT,
        altura        DOUBLE PRECISION,
        peso          DOUBLE PRECISION,
        animo         INT,
        notas_conducta TEXT,
        estado_fisico TEXT DEFAULT 'DISPONIBLE'
      )""")
      stmt.executeUpdate("ALTER TABLE wellness ADD COLUMN IF NOT EXISTS sueno_profundo_min INT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE wellness ADD COLUMN IF NOT EXISTS sueno_ligero_min INT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE wellness ADD COLUMN IF NOT EXISTS sueno_despierto_min INT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE wellness ADD COLUMN IF NOT EXISTS fc_reposo INT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE wellness ADD COLUMN IF NOT EXISTS somnolencia INT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE wellness ADD COLUMN IF NOT EXISTS dolor_muscular INT DEFAULT NULL")
      // Import FC por captura: exige una fila unica por fecha. logWellness ya guardaba
      // una fila nueva en cada guardado (sin upsert), asi que antes de forzar la
      // unicidad fusionamos duplicados historicos conservando la fila mas reciente.
      stmt.executeUpdate("DELETE FROM wellness w1 USING wellness w2 WHERE w1.fecha = w2.fecha AND w1.fecha IS NOT NULL AND w1.id < w2.id")
      stmt.executeUpdate("""
        DO $$
        BEGIN
          IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conname = 'wellness_fecha_unique') THEN
            ALTER TABLE wellness ADD CONSTRAINT wellness_fecha_unique UNIQUE (fecha);
          END IF;
        END $$;
      """)

      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS match_goals (
        id              SERIAL PRIMARY KEY,
        match_id        INT NOT NULL,
        minuto          INT DEFAULT 0,
        origen          TEXT,
        situacion       TEXT,
        responsabilidad TEXT DEFAULT 'Media',
        era_parable     TEXT DEFAULT 'Dudoso',
        zona_gol        TEXT,
        notas           TEXT
      )""")

      // BLOQUE D: desglose tecnico de paradas
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS paradas_detalle (
        id            SERIAL PRIMARY KEY,
        match_id      INT REFERENCES matches(id) ON DELETE CASCADE,
        numero_parada INT NOT NULL,
        tecnica       TEXT DEFAULT NULL,
        parte_cuerpo  TEXT DEFAULT NULL,
        resultado     TEXT DEFAULT NULL,
        zona_origen   TEXT DEFAULT NULL,
        created_at    TIMESTAMP DEFAULT NOW()
      )""")

      // BLOQUE RFFM: benchmarking real contra la categoria Prebenjamin F7
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS rffm_benchmark (
        id            SERIAL PRIMARY KEY,
        temporada     TEXT NOT NULL,
        competicion   TEXT NOT NULL,
        grupo         TEXT NOT NULL,
        nombre_grupo  TEXT NOT NULL,
        equipo_local  TEXT NOT NULL,
        equipo_visita TEXT NOT NULL,
        goles_local   INT NOT NULL,
        goles_visita  INT NOT NULL,
        jornada       INT NOT NULL,
        fecha         DATE DEFAULT NULL,
        created_at    TIMESTAMP DEFAULT NOW(),
        UNIQUE(competicion, grupo, equipo_local, equipo_visita, jornada)
      )""")
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS rffm_percentiles (
        id            SERIAL PRIMARY KEY,
        temporada     TEXT NOT NULL,
        competicion   TEXT NOT NULL,
        fecha_calculo DATE NOT NULL DEFAULT CURRENT_DATE,
        total_partidos INT DEFAULT 0,
        total_equipos  INT DEFAULT 0,
        media_gc      DOUBLE PRECISION DEFAULT 0,
        p10_gc        DOUBLE PRECISION DEFAULT 0,
        p25_gc        DOUBLE PRECISION DEFAULT 0,
        p50_gc        DOUBLE PRECISION DEFAULT 0,
        p75_gc        DOUBLE PRECISION DEFAULT 0,
        p90_gc        DOUBLE PRECISION DEFAULT 0,
        pct_limpias   DOUBLE PRECISION DEFAULT 0,
        created_at    TIMESTAMP DEFAULT NOW()
      )""")

      // BLOQUE E: hitos automaticos de carrera
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS hitos_conseguidos (
        id          SERIAL PRIMARY KEY,
        tipo        TEXT NOT NULL UNIQUE,
        descripcion TEXT NOT NULL,
        fecha       DATE NOT NULL DEFAULT CURRENT_DATE,
        contexto    TEXT DEFAULT '',
        created_at  TIMESTAMP DEFAULT NOW()
      )""")

      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS growth_history (
        id     SERIAL PRIMARY KEY,
        fecha  DATE DEFAULT CURRENT_DATE,
        altura DOUBLE PRECISION,
        peso   DOUBLE PRECISION
      )""")

      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS medical_records (
        id               SERIAL PRIMARY KEY,
        fecha            DATE,
        tipo             TEXT,
        diagnostico      TEXT,
        recomendaciones  TEXT,
        es_previo        BOOLEAN DEFAULT FALSE
      )""")

      // Entrenamiento y progresion tecnica
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS trainings (
        id             SERIAL PRIMARY KEY,
        fecha          DATE DEFAULT CURRENT_DATE,
        tipo           TEXT,
        foco           TEXT,
        rpe            INT,
        calidad        INT,
        atencion       INT,
        rutina_detalle TEXT
      )""")
      // Modulo 8: Audio-diario de sesiones de academia
      stmt.executeUpdate("ALTER TABLE trainings ADD COLUMN IF NOT EXISTS analisis_voz_academia TEXT DEFAULT ''")
      // BLOQUE B: Footbar en entrenamientos (opcional, sensor GPS)
      stmt.executeUpdate("ALTER TABLE trainings ADD COLUMN IF NOT EXISTS fb_distancia DOUBLE PRECISION DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE trainings ADD COLUMN IF NOT EXISTS fb_alta_intensidad INT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE trainings ADD COLUMN IF NOT EXISTS fb_sprint_max DOUBLE PRECISION DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE trainings ADD COLUMN IF NOT EXISTS fb_pct_actividad INT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE trainings ADD COLUMN IF NOT EXISTS fb_tiempo_activo INT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE trainings ADD COLUMN IF NOT EXISTS fb_aceleraciones INT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE trainings ADD COLUMN IF NOT EXISTS fb_desaceleraciones INT DEFAULT NULL")
      // BLOQUE N: duracion real de la sesion (la registra el bot de Telegram; la carga sigue usando 60*rpe)
      stmt.executeUpdate("ALTER TABLE trainings ADD COLUMN IF NOT EXISTS duracion_min INT DEFAULT NULL")
      // BLOQUE J: calibracion del padre como observador (situaciones generadas por Gemini al pulsar el boton)
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS calibracion_padre (
        id            SERIAL PRIMARY KEY,
        fecha         DATE DEFAULT CURRENT_DATE,
        situaciones   TEXT NOT NULL,
        puntuaciones  TEXT DEFAULT NULL,
        resultado     TEXT DEFAULT NULL,
        dimension     TEXT DEFAULT NULL,
        desviacion    DOUBLE PRECISION DEFAULT NULL,
        created_at    TIMESTAMP DEFAULT NOW()
      )""")
      // BLOQUE E: RPE percibido por Hector al llegar a casa (1=Fresco ... 5=Agotado)
      stmt.executeUpdate("ALTER TABLE trainings ADD COLUMN IF NOT EXISTS rpe_hector INT DEFAULT NULL")
      // BLOQUE N: estado de la conversacion con el bot de Telegram (un registro por chat)
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS telegram_session (
        chat_id           TEXT PRIMARY KEY,
        flujo             TEXT DEFAULT NULL,
        paso              TEXT DEFAULT NULL,
        match_id_temp     INT DEFAULT NULL,
        training_id_temp  INT DEFAULT NULL,
        goles_pendientes  INT DEFAULT 0,
        goles_registrados INT DEFAULT 0,
        updated_at        TIMESTAMP DEFAULT NOW()
      )""")
      // BLOQUE D: Vídeo IA en entrenamientos
      stmt.executeUpdate("ALTER TABLE trainings ADD COLUMN IF NOT EXISTS video_analisis_ia TEXT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE trainings ADD COLUMN IF NOT EXISTS video_analisis_fecha TIMESTAMP DEFAULT NULL")

      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS drills (
        id                SERIAL PRIMARY KEY,
        nombre            TEXT,
        descripcion       TEXT,
        sesiones_objetivo INT DEFAULT 10,
        sesiones_actuales INT DEFAULT 0,
        activo            BOOLEAN DEFAULT TRUE
      )""")

      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS technical_reviews (
        id             SERIAL PRIMARY KEY,
        fecha          DATE DEFAULT CURRENT_DATE,
        blocaje        INT,
        pies           INT,
        aereo          INT,
        valentia       INT,
        concentracion  INT,
        coordinacion   INT,
        notas          TEXT
      )""")

      // Material deportivo
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS gear (
        id                   SERIAL PRIMARY KEY,
        nombre               TEXT,
        tipo                 TEXT,
        vida_util_estimada   INT DEFAULT 30,
        usos_actuales        INT DEFAULT 0,
        activo               BOOLEAN DEFAULT TRUE,
        imagen_url           TEXT
      )""")
      stmt.executeUpdate("ALTER TABLE gear ADD COLUMN IF NOT EXISTS precio_compra DOUBLE PRECISION DEFAULT 0")

      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS injuries (
        id              SERIAL PRIMARY KEY,
        fecha_inicio    DATE DEFAULT CURRENT_DATE,
        fecha_alta      DATE,
        zona            TEXT,
        tipo            TEXT,
        gravedad        TEXT DEFAULT 'LEVE',
        descripcion     TEXT,
        dias_baja       INT DEFAULT 0,
        activa          BOOLEAN DEFAULT TRUE
      )""")
      stmt.executeUpdate("ALTER TABLE injuries ADD COLUMN IF NOT EXISTS tipo_clasificado TEXT DEFAULT 'OTRO'")
      stmt.executeUpdate("ALTER TABLE injuries ADD COLUMN IF NOT EXISTS lado TEXT DEFAULT 'NA'")
      stmt.executeUpdate("ALTER TABLE injuries ADD COLUMN IF NOT EXISTS causa_probable TEXT DEFAULT ''")
      stmt.executeUpdate("ALTER TABLE injuries ADD COLUMN IF NOT EXISTS partidos_perdidos INT DEFAULT 0")

      // Scouting y rivales
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS rivals (
        id              SERIAL PRIMARY KEY,
        nombre          TEXT UNIQUE,
        estilo_juego    TEXT,
        jugadores_clave TEXT,
        notas_scouting  TEXT
      )""")

      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS penalties (
        id          SERIAL PRIMARY KEY,
        fecha       DATE DEFAULT CURRENT_DATE,
        rival       TEXT,
        zona_tiro   TEXT,
        zona_salto  TEXT,
        es_gol      BOOLEAN DEFAULT FALSE
      )""")

      // Video y analisis
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS video_tags (
        id          SERIAL PRIMARY KEY,
        match_id    INT,
        minuto      INT,
        segundo     INT,
        tipo        TEXT,
        descripcion TEXT
      )""")

      // Objetivos y logros
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS objectives (
        id          SERIAL PRIMARY KEY,
        tipo        TEXT,
        objetivo    INT,
        descripcion TEXT
      )""")

      // Academico
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS academic_performance (
        id              SERIAL PRIMARY KEY,
        fecha           DATE DEFAULT CURRENT_DATE,
        asignatura      TEXT,
        nota            DOUBLE PRECISION,
        tipo_evaluacion TEXT,
        comentarios     TEXT
      )""")

      // Leyendas (comparativa historica)
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS legends_milestones (
        id     SERIAL PRIMARY KEY,
        nombre TEXT,
        edad   INT,
        hito   TEXT
      )""")

      // Cache de respuestas IA (evita llamadas duplicadas a Gemini)
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS ai_cache (
        prompt_hash TEXT PRIMARY KEY,
        respuesta   TEXT,
        creado_en   TIMESTAMP DEFAULT NOW()
      )""")
      // Limpiar errores cacheados de versiones anteriores en cada arranque
      stmt.executeUpdate("""DELETE FROM ai_cache WHERE
        respuesta LIKE 'Error:%' OR
        respuesta LIKE '%status code%' OR
        respuesta LIKE '%NOT_FOUND%' OR
        respuesta LIKE '%INVALID_ARGUMENT%' OR
        respuesta LIKE '%Error tras agotar%'
      """)

      // Columnas opcionales anadidas en versiones posteriores (ALTER IF NOT EXISTS es idempotente)
      stmt.executeUpdate("ALTER TABLE seasons ADD COLUMN IF NOT EXISTS judo_belt TEXT DEFAULT 'Blanco'")
      stmt.executeUpdate("ALTER TABLE seasons ADD COLUMN IF NOT EXISTS rffm_url TEXT")
      stmt.executeUpdate("ALTER TABLE seasons ADD COLUMN IF NOT EXISTS rffm_team_name TEXT")
      stmt.executeUpdate("ALTER TABLE seasons ADD COLUMN IF NOT EXISTS fecha_nacimiento DATE DEFAULT '2020-06-19'")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS mapa_campo TEXT")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS analisis_voz TEXT")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS lineas_superadas INT DEFAULT 0")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS acciones_preventivas INT DEFAULT 0")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS scanning_rate INT DEFAULT 0")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS es_local BOOLEAN DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS comportamiento_presion TEXT")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS nutricion_prepartido TEXT DEFAULT NULL")
      // Nutricion e hidratacion pre-partido (horas: 0=<1h, 1=1-2h, 2=2-3h, 3=>3h; hidratacion: BIEN/NORMAL/POCO)
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS horas_ultima_comida INT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS hidratacion_prepartido TEXT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS desayuno_completo BOOLEAN DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS video_analisis_ia TEXT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS video_analisis_fecha TIMESTAMP DEFAULT NULL")

      // ── v7.2: NLP Scouting Aggregator ──────────────────────────────────────
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS scouting_reports (
        id           SERIAL PRIMARY KEY,
        fecha        DATE DEFAULT CURRENT_DATE,
        ojeador      TEXT DEFAULT '',
        club_origen  TEXT DEFAULT '',
        texto_raw    TEXT NOT NULL,
        nivel_tecnico    INT DEFAULT 0,
        nivel_tactico    INT DEFAULT 0,
        nivel_fisico     INT DEFAULT 0,
        nivel_mental     INT DEFAULT 0,
        nivel_distribucion INT DEFAULT 0,
        nivel_global     INT DEFAULT 0,
        proyeccion       TEXT DEFAULT '',
        recomendacion    TEXT DEFAULT '',
        fortalezas       TEXT DEFAULT '',
        areas_mejora     TEXT DEFAULT '',
        resumen_ia       TEXT DEFAULT '',
        created_at   TIMESTAMP DEFAULT NOW()
      )""")

      // ── v7.2: Nutrition plans cache ─────────────────────────────────────────
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS nutrition_plans (
        id           SERIAL PRIMARY KEY,
        semana       DATE DEFAULT CURRENT_DATE,
        acwr         DOUBLE PRECISION DEFAULT 1.0,
        rpe_media    DOUBLE PRECISION DEFAULT 5.0,
        nota_ultimo  DOUBLE PRECISION DEFAULT 6.0,
        plan_ia      TEXT DEFAULT '',
        created_at   TIMESTAMP DEFAULT NOW()
      )""")

      // ── Footbar: sensor GPS de rendimiento fisico/tecnico (opcional por partido) ──
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS footbar_sessions (
        id                   SERIAL PRIMARY KEY,
        match_id             INT REFERENCES matches(id) ON DELETE CASCADE,
        distancia_km         DOUBLE PRECISION DEFAULT 0,
        alta_intensidad_m    DOUBLE PRECISION DEFAULT 0,
        sprint_max_kmh       DOUBLE PRECISION DEFAULT 0,
        pct_actividad        DOUBLE PRECISION DEFAULT 0,
        tiempo_actividad_min INT DEFAULT 0,
        aceleraciones        INT DEFAULT 0,
        desaceleraciones     INT DEFAULT 0,
        balones              INT DEFAULT 0,
        pases                INT DEFAULT 0,
        tiempo_balon_seg     INT DEFAULT 0,
        disparos             INT DEFAULT 0,
        tiro_max_kmh         DOUBLE PRECISION DEFAULT 0,
        created_at           TIMESTAMP DEFAULT NOW(),
        UNIQUE(match_id)
      )""")

      // ── Modulo 2: Checklist de habilidades tecnicas de portero ──────────────
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS goalkeeper_skills (
        id                   SERIAL PRIMARY KEY,
        categoria            TEXT NOT NULL,
        habilidad            TEXT NOT NULL,
        conseguido           BOOLEAN DEFAULT FALSE,
        fecha_conseguido     DATE,
        contexto_conseguido  TEXT,
        notas                TEXT DEFAULT ''
      )""")
      stmt.executeUpdate("ALTER TABLE goalkeeper_skills ADD COLUMN IF NOT EXISTS fecha_inicio_trabajo DATE")
      // BLOQUE N: niveles de automatismo (metodologia Ajax/Barca) — NULL/CONSCIENTE/AUTOMATICO/INSTINTIVO
      stmt.executeUpdate("ALTER TABLE goalkeeper_skills ADD COLUMN IF NOT EXISTS nivel_automatismo TEXT DEFAULT NULL")
      val rsSkillsCount = stmt.executeQuery("SELECT COUNT(*) FROM goalkeeper_skills")
      if (rsSkillsCount.next() && rsSkillsCount.getInt(1) == 0) {
        val seedSkills = Seq(
          ("Tecnica basica", "Posicion de manos correcta"),
          ("Tecnica basica", "Posicion de pies antes de recibir"),
          ("Tecnica basica", "Caida lateral derecha"),
          ("Tecnica basica", "Caida lateral izquierda"),
          ("Tecnica basica", "Saque con la mano rodada"),
          ("Tecnica basica", "Saque con la mano en volea"),
          ("Tecnica basica", "Despeje de puños"),
          ("Tecnica basica", "Blocaje de balon en carrera"),
          ("Juego con los pies", "Pase corto con el interior"),
          ("Juego con los pies", "Pase largo con el empeine"),
          ("Juego con los pies", "Control y conduccion bajo presion"),
          ("Juego con los pies", "Distribucion rapida"),
          ("Comportamiento en el area", "Manda en el area con voz"),
          ("Comportamiento en el area", "Sale a por balones aereos"),
          ("Comportamiento en el area", "Posicionamiento en tiros lejanos"),
          ("Comportamiento en el area", "Anticipacion en el 1v1"),
          ("Mental", "Concentracion tras error"),
          ("Mental", "Liderazgo y comunicacion con la defensa"),
          ("Mental", "Reaccion tras gol encajado"),
          ("Mental", "Constancia en el entreno")
        )
        val insSkill = conn.prepareStatement("INSERT INTO goalkeeper_skills (categoria, habilidad) VALUES (?, ?)")
        seedSkills.foreach { case (cat, hab) =>
          insSkill.setString(1, cat); insSkill.setString(2, hab); insSkill.executeUpdate()
        }
      }

      // ── Detector de ventanas sensibles de aprendizaje ────────────────────────
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS development_windows (
        id           SERIAL PRIMARY KEY,
        ventana      TEXT NOT NULL,
        edad_inicio  INT NOT NULL,
        edad_fin     INT NOT NULL,
        descripcion  TEXT DEFAULT ''
      )""")
      val rsWindowsCount = stmt.executeQuery("SELECT COUNT(*) FROM development_windows")
      if (rsWindowsCount.next() && rsWindowsCount.getInt(1) == 0) {
        val seedWindows = Seq(
          ("Coordinacion y equilibrio", 6, 8, "Ventana critica para caidas, lateralidad y propiocepcion"),
          ("Velocidad de reaccion", 7, 9, "Momento optimo para entrenar anticipacion y reflejos"),
          ("Tecnica con balon", 8, 11, "Mejor edad para automatizar gestos tecnicos con el pie"),
          ("Velocidad y agilidad", 9, 11, "Ventana para explosividad y cambios de direccion"),
          ("Fuerza relativa", 12, 14, "Inicio del trabajo de fuerza funcional"),
          ("Tactica colectiva", 11, 14, "Capacidad de abstraccion tactica desarrollada")
        )
        val insWindow = conn.prepareStatement("INSERT INTO development_windows (ventana, edad_inicio, edad_fin, descripcion) VALUES (?, ?, ?, ?)")
        seedWindows.foreach { case (v, ei, ef, d) =>
          insWindow.setString(1, v); insWindow.setInt(2, ei); insWindow.setInt(3, ef); insWindow.setString(4, d)
          insWindow.executeUpdate()
        }
      }

      // ── Modulo 3: Registro de visibilidad y oportunidades ───────────────────
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS opportunities (
        id                     SERIAL PRIMARY KEY,
        fecha                  DATE DEFAULT CURRENT_DATE,
        tipo                   TEXT NOT NULL,
        descripcion            TEXT DEFAULT '',
        club_o_entidad         TEXT DEFAULT '',
        resultado              TEXT DEFAULT '',
        seguimiento            TEXT DEFAULT '',
        seguimiento_completado BOOLEAN DEFAULT FALSE,
        created_at             TIMESTAMP DEFAULT NOW()
      )""")

      // ── Cache generico de features IA con expiracion por tiempo (7 dias) ────
      // Reutilizado por el Indice de Resiliencia (Modulo 4) y el Benchmark (Modulo 5)
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS feature_cache (
        cache_key  TEXT PRIMARY KEY,
        payload    TEXT NOT NULL,
        updated_at TIMESTAMP DEFAULT NOW()
      )""")

      // ── Modulo 7: Red de contactos (mini CRM) ────────────────────────────────
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS contacts (
        id                   SERIAL PRIMARY KEY,
        nombre               TEXT NOT NULL,
        rol                  TEXT NOT NULL DEFAULT 'OTRO',
        club_o_entidad       TEXT DEFAULT '',
        telefono             TEXT DEFAULT '',
        email                TEXT DEFAULT '',
        como_conocido        TEXT DEFAULT '',
        ultima_interaccion   DATE,
        notas                TEXT DEFAULT '',
        importancia          TEXT NOT NULL DEFAULT 'MEDIA',
        created_at           TIMESTAMP DEFAULT NOW()
      )""")
      stmt.executeUpdate("ALTER TABLE opportunities ADD COLUMN IF NOT EXISTS contact_id INT")

      // ── Modulo 4 (sesion actual): Mapa de visibilidad y eventos clave ────────
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS visibility_events (
        id                   SERIAL PRIMARY KEY,
        nombre               TEXT NOT NULL,
        fecha                DATE NOT NULL,
        tipo                 TEXT NOT NULL DEFAULT 'OTRO',
        organizador          TEXT DEFAULT '',
        nivel_visibilidad    TEXT NOT NULL DEFAULT 'MEDIO',
        participamos         BOOLEAN DEFAULT FALSE,
        ojeadores_presentes  BOOLEAN,
        contact_id           INT,
        notas                TEXT DEFAULT '',
        created_at           TIMESTAMP DEFAULT NOW()
      )""")
      val rsVisCount = stmt.executeQuery("SELECT COUNT(*) FROM visibility_events")
      if (rsVisCount.next() && rsVisCount.getInt(1) == 0) {
        val seedEvents = Seq(
          ("Torneo de Navidad RFFM", "2026-12-20", "TORNEO", "RFFM", "ALTO", ""),
          ("Torneo de Semana Santa", "2027-03-29", "TORNEO", "Club organizador", "ALTO", ""),
          ("Copa Federación Madrid Prebenjamín", "2027-05-15", "LIGA_REGIONAL", "RFFM", "MEDIO", ""),
          ("Campus de verano ATM/Rayo/Getafe", "2027-07-06", "CAMPUS", "Multiclub", "MEDIO", ""),
          ("Jornada de detección RFFM", "2027-02-07", "PRUEBA_CLUB", "RFFM", "ALTO", "Fecha estimada — confirmar con RFFM, suele variar cada temporada")
        )
        val insVis = conn.prepareStatement(
          "INSERT INTO visibility_events (nombre, fecha, tipo, organizador, nivel_visibilidad, notas) VALUES (?, ?::date, ?, ?, ?, ?)"
        )
        seedEvents.foreach { case (n, f, t, o, nv, notas) =>
          insVis.setString(1, n); insVis.setString(2, f); insVis.setString(3, t)
          insVis.setString(4, o); insVis.setString(5, nv); insVis.setString(6, notas)
          insVis.executeUpdate()
        }
      }

      // ── Modulo 5: Diario narrativo automatico de temporada ───────────────────
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS season_diary (
        id                   SERIAL PRIMARY KEY,
        mes                  TEXT NOT NULL UNIQUE,
        contenido            TEXT NOT NULL,
        generado_en          TIMESTAMP DEFAULT NOW(),
        partidos_incluidos   INT DEFAULT 0,
        hitos_incluidos      INT DEFAULT 0
      )""")
      // Diario narrativo: contexto con el que se genero cada mes (datos_mes)
      stmt.executeUpdate("ALTER TABLE season_diary ADD COLUMN IF NOT EXISTS datos_mes TEXT DEFAULT NULL")

      // ── Modulo 6: Periodizacion anual ────────────────────────────────────────
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS periodization (
        id           SERIAL PRIMARY KEY,
        nombre       TEXT NOT NULL,
        fecha_inicio DATE NOT NULL,
        fecha_fin    DATE NOT NULL,
        tipo         TEXT NOT NULL,
        notas        TEXT DEFAULT '',
        color        TEXT DEFAULT '#6c757d'
      )""")

      // ── D2: Registro psicologico trimestral ──────────────────────────────────
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS psych_records (
        id                   SERIAL PRIMARY KEY,
        fecha                DATE NOT NULL DEFAULT CURRENT_DATE,
        motivacion           INT NOT NULL CHECK (motivacion BETWEEN 1 AND 5),
        presion_percibida    INT NOT NULL CHECK (presion_percibida BETWEEN 1 AND 5),
        relacion_errores     INT NOT NULL CHECK (relacion_errores BETWEEN 1 AND 5),
        miedo_fracaso        INT NOT NULL CHECK (miedo_fracaso BETWEEN 1 AND 5),
        disfrute             INT NOT NULL CHECK (disfrute BETWEEN 1 AND 5),
        relacion_entrenador  INT NOT NULL CHECK (relacion_entrenador BETWEEN 1 AND 5),
        relacion_equipo      INT NOT NULL CHECK (relacion_equipo BETWEEN 1 AND 5),
        notas                TEXT DEFAULT '',
        created_at           TIMESTAMP DEFAULT NOW()
      )""")

      // ── C3: Test fisicos trimestrales ────────────────────────────────────────
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS physical_tests (
        id                       SERIAL PRIMARY KEY,
        fecha                    DATE NOT NULL DEFAULT CURRENT_DATE,
        velocidad_10m            DOUBLE PRECISION DEFAULT NULL,
        velocidad_30m            DOUBLE PRECISION DEFAULT NULL,
        salto_vertical_cm        INT DEFAULT NULL,
        agilidad_illinois_s      DOUBLE PRECISION DEFAULT NULL,
        lanzamiento_medicinal_cm INT DEFAULT NULL,
        notas                    TEXT DEFAULT '',
        created_at               TIMESTAMP DEFAULT NOW()
      )""")

      // ─────────────────────────────────────────────────────────────────────────────
      // BLOQUE B1 — IDP: PLAN DE DESARROLLO INDIVIDUAL
      // ─────────────────────────────────────────────────────────────────────────────
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS idp_temporadas (
        id            SERIAL PRIMARY KEY,
        temporada     TEXT NOT NULL,
        fecha_inicio  DATE NOT NULL,
        fecha_fin     DATE NOT NULL,
        estado        TEXT DEFAULT 'ACTIVA',
        resumen_ia    TEXT DEFAULT '',
        created_at    TIMESTAMP DEFAULT NOW()
      )""")

      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS idp_objetivos (
        id              SERIAL PRIMARY KEY,
        temporada_id    INT REFERENCES idp_temporadas(id) ON DELETE CASCADE,
        dimension       TEXT NOT NULL,
        objetivo        TEXT NOT NULL,
        metrica         TEXT NOT NULL,
        valor_actual    TEXT DEFAULT '',
        valor_objetivo  TEXT NOT NULL,
        fecha_limite    DATE NOT NULL,
        progreso_pct    INT DEFAULT 0,
        estado          TEXT DEFAULT 'EN_CURSO',
        notas           TEXT DEFAULT '',
        created_at      TIMESTAMP DEFAULT NOW()
      )""")

      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS idp_revisiones (
        id              SERIAL PRIMARY KEY,
        temporada_id    INT REFERENCES idp_temporadas(id) ON DELETE CASCADE,
        fecha           DATE NOT NULL DEFAULT CURRENT_DATE,
        tipo            TEXT DEFAULT 'MENSUAL',
        resumen         TEXT DEFAULT '',
        ajustes         TEXT DEFAULT '',
        analisis_ia     TEXT DEFAULT '',
        created_at      TIMESTAMP DEFAULT NOW()
      )""")

      // ─────────────────────────────────────────────────────────────────────────────
      // BLOQUE A1 — INDICE DE FORMA DIARIO
      // ─────────────────────────────────────────────────────────────────────────────
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS forma_diaria (
        id              SERIAL PRIMARY KEY,
        fecha           DATE NOT NULL DEFAULT CURRENT_DATE,
        indice_forma    DOUBLE PRECISION NOT NULL,
        sueno_score     DOUBLE PRECISION DEFAULT 0,
        energia_score   DOUBLE PRECISION DEFAULT 0,
        animo_score     DOUBLE PRECISION DEFAULT 0,
        acwr_score      DOUBLE PRECISION DEFAULT 0,
        descanso_score  DOUBLE PRECISION DEFAULT 0,
        phv_score       DOUBLE PRECISION DEFAULT 0,
        nota_partido    DOUBLE PRECISION DEFAULT NULL,
        match_id        INT REFERENCES matches(id) ON DELETE SET NULL,
        UNIQUE(fecha)
      )""")

      // ─────────────────────────────────────────────────────────────────────────────
      // BLOQUE B1 — INDICE DE COGNICION ANTICIPATORIA
      // ─────────────────────────────────────────────────────────────────────────────
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS cognitivo_tests (
        id                    SERIAL PRIMARY KEY,
        fecha                 DATE NOT NULL DEFAULT CURRENT_DATE,
        reaccion_aciertos     INT DEFAULT NULL,
        reaccion_total        INT DEFAULT 10,
        lectura_senales       INT DEFAULT NULL,
        velocidad_decision    INT DEFAULT NULL,
        pausa_cognitiva       INT DEFAULT NULL,
        notas                 TEXT DEFAULT '',
        created_at            TIMESTAMP DEFAULT NOW()
      )""")

      // ─────────────────────────────────────────────────────────────────────────────
      // BLOQUE C1 — URL PUBLICA CONTROLADA DE HECTOR
      // ─────────────────────────────────────────────────────────────────────────────
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS perfil_publico (
        id                SERIAL PRIMARY KEY,
        activo            BOOLEAN DEFAULT FALSE,
        password_lectura  TEXT DEFAULT '',
        mostrar_carta     BOOLEAN DEFAULT TRUE,
        mostrar_progresion BOOLEAN DEFAULT TRUE,
        mostrar_video_ia  BOOLEAN DEFAULT TRUE,
        mostrar_idp       BOOLEAN DEFAULT TRUE,
        mostrar_informe   BOOLEAN DEFAULT TRUE,
        mostrar_cognitivo BOOLEAN DEFAULT FALSE,
        mostrar_medico    BOOLEAN DEFAULT FALSE,
        visitas           INT DEFAULT 0,
        ultima_visita     TIMESTAMP DEFAULT NULL,
        created_at        TIMESTAMP DEFAULT NOW()
      )""")
      stmt.executeUpdate("INSERT INTO perfil_publico (activo) SELECT FALSE WHERE NOT EXISTS (SELECT 1 FROM perfil_publico)")
      // BLOQUE ARQUETIPO: mostrar las 4 barras de arquetipo en el perfil publico
      stmt.executeUpdate("ALTER TABLE perfil_publico ADD COLUMN IF NOT EXISTS mostrar_arquetipo BOOLEAN DEFAULT TRUE")
      // MODULO LA VOZ DEL PORTERO: mostrar solo la carita y la tendencia, nunca el texto
      stmt.executeUpdate("ALTER TABLE perfil_publico ADD COLUMN IF NOT EXISTS mostrar_voz_portero BOOLEAN DEFAULT FALSE")

      // ── LA VOZ DEL PORTERO — registro mensual en primera persona de Hector ───
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS voz_portero (
        id                    SERIAL PRIMARY KEY,
        fecha                 DATE NOT NULL DEFAULT CURRENT_DATE,
        motivacion_carita     INT NOT NULL CHECK (motivacion_carita BETWEEN 1 AND 5),
        respuesta_error       TEXT NOT NULL,
        respuesta_aprendizaje TEXT NOT NULL,
        analisis_ia           TEXT DEFAULT NULL,
        analisis_fecha        TIMESTAMP DEFAULT NULL,
        created_at            TIMESTAMP DEFAULT NOW()
      )""")
      // UNIQUE sobre una expresion (mes de la fecha) no es un constraint de tabla valido en
      // Postgres — se implementa como indice unico sobre la expresion, y sirve igualmente
      // como target de ON CONFLICT para el upsert "un registro por mes".
      // OJO: DATE_TRUNC('month', fecha) sobre un DATE resuelve a la variante timestamptz, que es
      // STABLE (depende de la zona horaria) y Postgres la rechaza en un indice. El cast a
      // ::timestamp usa la variante IMMUTABLE. saveVozPortero() debe usar la misma expresion.
      stmt.executeUpdate(
        "CREATE UNIQUE INDEX IF NOT EXISTS voz_portero_mes_idx ON voz_portero (DATE_TRUNC('month', fecha::timestamp))")

      // ── ARQUETIPO DE PORTERO — historico mensual (SQL puro, sin Gemini) ──────
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS arquetipo_history (
        id              SERIAL PRIMARY KEY,
        season_id       INT REFERENCES seasons(id) ON DELETE CASCADE,
        fecha_calculo   DATE NOT NULL DEFAULT CURRENT_DATE,
        pct_sweeper     INT DEFAULT 0,
        pct_shot_stopper INT DEFAULT 0,
        pct_commanding  INT DEFAULT 0,
        pct_modern      INT DEFAULT 0,
        arquetipo_dominante TEXT NOT NULL,
        arquetipo_secundario TEXT DEFAULT NULL,
        partidos_base   INT DEFAULT 0,
        created_at      TIMESTAMP DEFAULT NOW()
      )""")

      // ─────────────────────────────────────────────────────────────────────────────
      // MODULO — SISTEMA DE BACKUPS AUTOMATICOS
      // ─────────────────────────────────────────────────────────────────────────────
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS backups_log (
        id          SERIAL PRIMARY KEY,
        fecha       DATE NOT NULL DEFAULT CURRENT_DATE,
        tamano_kb   INT DEFAULT 0,
        sql_dump    TEXT NOT NULL,
        destinos    TEXT DEFAULT '',
        created_at  TIMESTAMP DEFAULT NOW()
      )""")

      // ─────────────────────────────────────────────────────────────────────────────
      // BLOQUE 2 — GESTION COMPLETA DE TEMPORADAS
      // ─────────────────────────────────────────────────────────────────────────────
      stmt.executeUpdate("ALTER TABLE seasons ADD COLUMN IF NOT EXISTS nombre TEXT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE seasons ADD COLUMN IF NOT EXISTS fecha_fin DATE DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE seasons ADD COLUMN IF NOT EXISTS media_final DOUBLE PRECISION DEFAULT 0")
      stmt.executeUpdate("ALTER TABLE seasons ADD COLUMN IF NOT EXISTS porterias_cero_total INT DEFAULT 0")
      stmt.executeUpdate("ALTER TABLE seasons ADD COLUMN IF NOT EXISTS informe_fin_temporada TEXT DEFAULT NULL")

      // ─────────────────────────────────────────────────────────────────────────────
      // BLOQUE 3 — REGISTRO DIARIO Y WELLNESS
      // ─────────────────────────────────────────────────────────────────────────────
      // physical_growth es la tabla realmente usada por la app (growth_history es legacy/no usada);
      // se crea defensivamente por si el CREATE original ya no esta en el historial de migraciones.
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS physical_growth (
        id                  SERIAL PRIMARY KEY,
        fecha               DATE DEFAULT CURRENT_DATE,
        altura              DOUBLE PRECISION,
        peso                DOUBLE PRECISION,
        velocidad_crecimiento DOUBLE PRECISION DEFAULT 0
      )""")
      // PHV Mirwald: antropometria adicional para maturity offset (antes iba antes del CREATE
      // y en una base de datos nueva initDB fallaba con "relation physical_growth does not exist")
      stmt.executeUpdate("ALTER TABLE physical_growth ADD COLUMN IF NOT EXISTS talla_sentado_cm DOUBLE PRECISION DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE physical_growth ADD COLUMN IF NOT EXISTS longitud_pierna_cm DOUBLE PRECISION DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE physical_growth ADD COLUMN IF NOT EXISTS kg_musculo DOUBLE PRECISION DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE physical_growth ADD COLUMN IF NOT EXISTS kg_masa_osea DOUBLE PRECISION DEFAULT NULL")

      // ─────────────────────────────────────────────────────────────────────────────
      // BLOQUE 4 — MEJORAS EN EL REGISTRO DE PARTIDO
      // ─────────────────────────────────────────────────────────────────────────────
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS rubrica_posicion INT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS rubrica_decisiones INT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS rubrica_pies INT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS rubrica_comunicacion INT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS rubrica_actitud INT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE trainings ADD COLUMN IF NOT EXISTS feedback_entrenador TEXT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS posicion_set TEXT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS altura_bloque TEXT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS pie_no_dominante_acciones INT DEFAULT 0")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS iniciativa_vocal TEXT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS guia_conversacion TEXT DEFAULT NULL")
      // BLOQUE B: autopercepcion de Hector pre-partido (1-5)
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS autopercepcion_prepartido INT DEFAULT NULL")
      // BLOQUE C: contexto avanzado del partido
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS calentamiento_min INT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS calentamiento_tipo TEXT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS superficie TEXT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS factores_externos TEXT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS velocidad_distribucion TEXT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS economia_movimiento INT DEFAULT NULL")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS calidad_decision_pct INT DEFAULT NULL")
      // BLOQUE H: indice de rendimiento contextual (CPI)
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS cpi DOUBLE PRECISION DEFAULT NULL")
      // BLOQUE B: autoevaluacion de la conducta del padre en la banda (1-5, privado, nunca publico)
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS conducta_padre INT DEFAULT NULL")
      // BLOQUE G: efectividad del scanning (amplia scanning_rate existente)
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS scanning_efectivo INT DEFAULT 0")
      // BLOQUE H: exito en 1v1 por angulo de entrada (JSON)
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS angulo_1v1_data TEXT DEFAULT NULL")
      // BLOQUE O: rutina pre-partido
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS rutina_prepartido TEXT DEFAULT NULL")
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS rutina_definicion (
        id          SERIAL PRIMARY KEY,
        descripcion TEXT NOT NULL,
        activa      BOOLEAN DEFAULT TRUE,
        created_at  TIMESTAMP DEFAULT NOW()
      )""")
      // BLOQUE Q: rendimiento por fase del partido (JSON goles por cuarto)
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS minuto_goles TEXT DEFAULT NULL")
      // BLOQUE R: tests de movilidad especificos de portero
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS movilidad_tests (
        id                      SERIAL PRIMARY KEY,
        fecha                   DATE NOT NULL DEFAULT CURRENT_DATE,
        alcance_pie_cm          INT DEFAULT NULL,
        rotacion_hombro         TEXT DEFAULT NULL,
        alcance_lateral_der_cm  INT DEFAULT NULL,
        alcance_lateral_izq_cm  INT DEFAULT NULL,
        asimetria_lateral_cm    INT DEFAULT NULL,
        notas                   TEXT DEFAULT '',
        created_at              TIMESTAMP DEFAULT NOW()
      )""")
      // BLOQUE S: toolkit de regulacion emocional
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS regulacion_emocional TEXT DEFAULT NULL")

      // BLOQUE C: calendario de carga cognitiva escolar
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS calendario_escolar (
        id           SERIAL PRIMARY KEY,
        fecha_inicio DATE NOT NULL,
        fecha_fin    DATE NOT NULL,
        tipo         TEXT NOT NULL,
        descripcion  TEXT DEFAULT '',
        created_at   TIMESTAMP DEFAULT NOW()
      )""")

      // ─────────────────────────────────────────────────────────────────────────────
      // BLOQUE 5.4 — MICRO-OBJETIVOS SEMANALES
      // ─────────────────────────────────────────────────────────────────────────────
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS micro_objetivos (
        id              SERIAL PRIMARY KEY,
        semana_inicio   DATE NOT NULL,
        objetivo_semana TEXT NOT NULL,
        dimension_idp   TEXT DEFAULT '',
        completado      BOOLEAN DEFAULT FALSE,
        resultado       TEXT DEFAULT '',
        generado_ia     BOOLEAN DEFAULT FALSE,
        created_at      TIMESTAMP DEFAULT NOW(),
        UNIQUE(semana_inicio)
      )""")

      // ─────────────────────────────────────────────────────────────────────────────
      // BLOQUE B — ESTRUCTURA SEMANAL FIJA DE HECTOR (Elite exclusivamente)
      // ─────────────────────────────────────────────────────────────────────────────
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS weekly_structure (
        id          SERIAL PRIMARY KEY,
        dia_semana  INT NOT NULL CHECK (dia_semana BETWEEN 1 AND 7),
        tipo_sesion TEXT NOT NULL,
        activo      BOOLEAN DEFAULT TRUE,
        notas       TEXT DEFAULT '',
        UNIQUE(dia_semana, tipo_sesion)
      )""")
      stmt.executeUpdate("""
        INSERT INTO weekly_structure (dia_semana, tipo_sesion, notas) VALUES
          (1, 'JUDO',     'Lunes — Judo'),
          (2, 'EQUIPO',   'Martes — Entreno equipo'),
          (3, 'JUDO',     'Miercoles — Judo'),
          (4, 'EQUIPO',   'Jueves — Entreno equipo'),
          (6, 'PARTIDO',  'Sabado — Partido'),
          (7, 'ACADEMIA', 'Domingo — Academia porteros')
        ON CONFLICT (dia_semana, tipo_sesion) DO NOTHING
      """)

      // ─────────────────────────────────────────────────────────────────────────────
      // BLOQUE B — CONTRACT & LICENSE VAULT (Elite exclusivamente)
      // ─────────────────────────────────────────────────────────────────────────────
      stmt.executeUpdate("""CREATE TABLE IF NOT EXISTS document_vault (
        id          SERIAL PRIMARY KEY,
        tipo        TEXT NOT NULL,
        nombre      TEXT NOT NULL,
        fecha       DATE DEFAULT CURRENT_DATE,
        archivo_b64 TEXT NOT NULL,
        notas       TEXT DEFAULT '',
        created_at  TIMESTAMP DEFAULT NOW()
      )""")

      // ─────────────────────────────────────────────────────────────────────────────
      // BLOQUE C — SET-PIECE CONTROL (Elite exclusivamente)
      // ─────────────────────────────────────────────────────────────────────────────
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS corners_dominados INT DEFAULT 0")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS corners_cedidos INT DEFAULT 0")
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS faltas_area_dominadas INT DEFAULT 0")

      // ─────────────────────────────────────────────────────────────────────────────
      // PROBLEMA 3 — MARCAR SESION COMO "NO ASISTIO" (ausencia intencional a la estructura semanal)
      // ─────────────────────────────────────────────────────────────────────────────
      stmt.executeUpdate("ALTER TABLE trainings ADD COLUMN IF NOT EXISTS tipo_ausencia TEXT DEFAULT NULL")

      // BLOQUE D/S: origen del registro del partido — NULL (formulario completo), 'quick' (registro minimo), 'importado' (CSV historico)
      stmt.executeUpdate("ALTER TABLE matches ADD COLUMN IF NOT EXISTS source TEXT DEFAULT NULL")

      println("[OK] initDB: todas las tablas verificadas.")
    } catch {
      case e: Exception => println(s"[!] initDB error: ${e.getMessage}")
    } finally {
      conn.close()
    }
  }

  // BLOQUE H1: mismo fix que SharedLayout.fixEncoding — solo re-encodea ante secuencias
  // concretas de corrupcion UTF-8/ISO-8859-1, nunca por contener una simple "A" mayuscula.
  def fixEncoding(s: String): String = {
    if (s == null || s.isEmpty) return ""
    try {
      val indicadores = Seq("Ã±", "Ã¡", "Ã©", "Ã³", "Ã", "Ãº", "Ã¼", "Ã ", "Ã¨", "Ã¬", "Ã²", "Ã€")
      if (indicadores.exists(s.contains)) new String(s.getBytes("ISO-8859-1"), "UTF-8")
      else s
    } catch { case _: Exception => s }
  }

  /** Escapa caracteres HTML peligrosos en strings que provienen de la BD
   *  y van a ser embebidos en HTML (raw()). Previene XSS.
   *  Usalo siempre que hagas: s"...$variableDeBD..." dentro de un bloque HTML.
   */
  def escHtml(s: String): String = {
    if (s == null) ""
    else s.replace("&", "&amp;")
          .replace("<", "&lt;")
          .replace(">", "&gt;")
          .replace("\"", "&quot;")
          .replace("'", "&#x27;")
  }
  def calcularEdadExacta(fechaStr: String): Int = { try { Period.between(LocalDate.parse(fechaStr), LocalDate.now()).getYears } catch { case _: Exception => 5 } }

  // --- NUEVO: SISTEMA DE AUDITORIA TECNICA ---
  def saveTechnicalReview(blocaje: Int, pies: Int, aereo: Int, valentia: Int, concentracion: Int, coordinacion: Int, notas: String): Unit = {
    val conn = getConnection(); try {
      val ps = conn.prepareStatement("INSERT INTO technical_reviews (fecha, blocaje, pies, aereo, valentia, concentracion, coordinacion, notas) VALUES (?,?,?,?,?,?,?,?)")
      ps.setDate(1, Date.valueOf(LocalDate.now()))
      ps.setInt(2, blocaje); ps.setInt(3, pies); ps.setInt(4, aereo); ps.setInt(5, valentia); ps.setInt(6, concentracion); ps.setInt(7, coordinacion)
      ps.setString(8, fixEncoding(notas))
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def getTechnicalReviews(): List[TechReview] = {
    var l = List[TechReview](); val conn = getConnection(); try {
      val rs = conn.createStatement().executeQuery("SELECT * FROM technical_reviews ORDER BY fecha ASC")
      while(rs.next()) {
        l = l :+ TechReview(rs.getInt("id"), rs.getDate("fecha").toString, rs.getInt("blocaje"), rs.getInt("pies"), rs.getInt("aereo"), rs.getInt("valentia"), rs.getInt("concentracion"), rs.getInt("coordinacion"), rs.getString("notas"))
      }
    } finally { conn.close() }; l
  }

  def getTechEvolutionChart(): String = {
    val reviews = getTechnicalReviews()
    if (reviews.isEmpty) return "{ \"labels\": [], \"datasets\": [] }"
    val labels = reviews.map(r => s"'${r.fecha}'").mkString(",")
    val d1 = reviews.map(_.blocaje).mkString(",")
    val d2 = reviews.map(_.valentia).mkString(",")
    val d3 = reviews.map(_.concentracion).mkString(",")
    s"""{
      "labels": [$labels],
      "datasets": [
        { "label": "Blocaje/Manos", "data": [$d1], "borderColor": "#0dcaf0", "tension": 0.3, "fill": false },
        { "label": "Valentia", "data": [$d2], "borderColor": "#dc3545", "tension": 0.3, "fill": false },
        { "label": "Concentracion", "data": [$d3], "borderColor": "#ffc107", "tension": 0.3, "fill": false }
      ]
    }"""
  }

  // --- LEYENDAS Y COMPARATIVA (Mantenido) ---
  def initLegendsTable(): String = {
    val conn = getConnection(); try {
      val stmt = conn.createStatement()
      stmt.executeUpdate("DELETE FROM legends_milestones")
      // Corregir nombres corruptos que puedan existir en DB de versiones anteriores
      val fixStmt = conn.createStatement()
      fixStmt.execute("UPDATE legends_milestones SET nombre = 'Marc-Andre ter Stegen' WHERE nombre LIKE 'Marc-Andr%ter Stegen'")
      fixStmt.execute("UPDATE legends_milestones SET hito = REPLACE(hito, 'a\u00f1os', 'anos') WHERE hito LIKE '%a_os%'")
      fixStmt.close()

      val ps = conn.prepareStatement("INSERT INTO legends_milestones (nombre, edad, hito) VALUES (?,?,?)")
      val data = Seq(("Marc-Andre ter Stegen", 5, "Jugaba de DELANTERO. No se puso de portero hasta los 10 anos."), ("Thibaut Courtois", 5, "Su deporte principal era el VOLEIBOL."), ("Iker Casillas", 6, "Jugaba en el patio del colegio El Recuerdo sobre cemento."), ("Gianluigi Buffon", 6, "Jugaba de centrocampista. Le gustaba correr y marcar goles."), ("Manuel Neuer", 5, "Llevaba un osito de peluche a la porteria."))
      data.foreach { case (n, e, h) => ps.setString(1, n); ps.setInt(2, e); ps.setString(3, fixEncoding(h)); ps.executeUpdate() }
      "Base de datos de Leyendas actualizada."
    } catch { case e: Exception => s"Error: ${e.getMessage}" } finally { conn.close() }
  }

  def getLegendComparison(): String = {
    val card = getLatestCardData(); val edad = calcularEdadExacta(card.fechaNacimiento); val conn = getConnection()
    try {
      val rsLeague = conn.createStatement().executeQuery("SELECT AVG(goles_contra) as media_liga FROM matches WHERE status='PLAYED'")
      var mediaLiga = 0.0; if(rsLeague.next()) mediaLiga = rsLeague.getDouble("media_liga")
      val rsMyStats = conn.createStatement().executeQuery("SELECT AVG(goles_contra) as mi_media FROM matches WHERE status='PLAYED'")
      var miMedia = 0.0; if(rsMyStats.next()) miMedia = rsMyStats.getDouble("mi_media")
      val rsLegend = conn.createStatement().executeQuery(s"SELECT * FROM legends_milestones WHERE edad <= $edad ORDER BY edad DESC LIMIT 1")
      val legendHtml = if (rsLegend.next()) {
        val safeNombre = escHtml(fixEncoding(rsLegend.getString("nombre")))
        val safeHito   = escHtml(fixEncoding(rsLegend.getString("hito")))
        "<div class='mb-3'>" +
        "<h6 class='text-warning text-uppercase mb-1'>A TU EDAD (" + edad + " A\u00d1OS)...</h6>" +
        "<h4 class='text-white fw-bold mb-1'>" + safeNombre + "</h4>" +
        "<p class='text-light small fst-italic'>&quot;" + safeHito + "&quot;</p>" +
        "</div>"
      } else ""
      val diff = mediaLiga - miMedia; val color = if(diff >= 0) "text-success" else "text-danger"
      f"""<div class="card bg-secondary bg-opacity-10 border-warning shadow mb-4"><div class="card-header bg-dark text-warning fw-bold text-center small">CONTEXTO & LEYENDAS</div><div class="card-body">$legendHtml<hr class="border-secondary"><h6 class="text-info text-uppercase text-center mb-2 small fw-bold">COMPARATIVA RFFM</h6><div class="row text-center align-items-center"><div class="col-6 border-end border-secondary"><div class="small text-muted fw-bold">TU MEDIA</div><div class="display-6 fw-bold $color">${f"$miMedia%1.1f"}</div></div><div class="col-6"><div class="small text-muted fw-bold">MEDIA LIGA</div><div class="display-6 fw-bold text-white">${f"$mediaLiga%1.1f"}</div></div></div></div></div>"""
    } catch { case e: Exception => "" } finally { conn.close() }
  }

  // --- IA CONFIG ---
  val modelList = Seq("gemini-2.5-flash", "gemini-flash-latest")
  object AIProvider {
    import java.security.MessageDigest

    private def getHash(s: String): String =
      MessageDigest.getInstance("SHA-256").digest(s.getBytes("UTF-8")).map("%02x".format(_)).mkString

    // Funcion principal: intenta cache, si no, llama a Gemini
    def ask(prompt: String, media: Option[(String, String)] = None, bypassCache: Boolean = false): String = {
      val conn = DatabaseManager.getConnection()
      // Creamos un hash unico combinando el prompt y los primeros bytes del archivo (si existe)
      val combinedKey = prompt + media.map(_._2.take(100)).getOrElse("")
      val hash = getHash(combinedKey)

      try {
        if (!bypassCache) {
          val ps = conn.prepareStatement("SELECT respuesta FROM ai_cache WHERE prompt_hash = ?")
          ps.setString(1, hash)
          val rs = ps.executeQuery()
          if (rs.next()) return rs.getString("respuesta")
        }

        // Llamada real a la API (Unificada)
        val response = callGeminiUnified(prompt, media)

        // Guardar en cache solo si la respuesta es valida (no un error)
        if (!response.startsWith("Error:") && !response.contains("status code") && !response.contains("NOT_FOUND")) {
          val save = conn.prepareStatement(
            "INSERT INTO ai_cache (prompt_hash, respuesta) VALUES (?, ?) ON CONFLICT (prompt_hash) DO UPDATE SET respuesta = EXCLUDED.respuesta"
          )
          save.setString(1, hash)
          save.setString(2, response)
          save.executeUpdate()
        } else {
          // Limpiar cualquier error cacheado anteriormente para este hash
          val del = conn.prepareStatement("DELETE FROM ai_cache WHERE prompt_hash = ?")
          del.setString(1, hash)
          del.executeUpdate()
        }

        response
      } finally { conn.close() }
    }

    private def callGeminiUnified(prompt: String, media: Option[(String, String)]): String = {
      val apiKey = sys.env.getOrElse("GEMINI_API_KEY", "").trim
      if (apiKey.isEmpty) return "Error: GEMINI_API_KEY no configurada"

      val isPdf = media.exists(_._1 == "application/pdf")
      val isVideo = media.exists(_._1.startsWith("video/"))

      // Usar siempre v1beta — soporta PDF y es compatible con cualquier API key de Google AI Studio
      val urls = Seq(
        s"https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent?key=$apiKey",
        // s"https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash-exp:generateContent?key=$apiKey",
        //s"https://generativelanguage.googleapis.com/v1beta/models/:generateContent?key=$apiKey"
      )

      if (debugMode) println(s"DEBUG: isPdf=$isPdf key=[${apiKey.take(4)}...${apiKey.takeRight(4)}]")

      val parts = ujson.Arr(ujson.Obj("text" -> prompt))

      media.foreach { case (mime, data) =>
        val cleanData = if (data.contains(",")) data.split(",")(1) else data
        if (mime == "application/pdf") {
          // PDFs: inlineData con mime correcto — funciona en gemini-1.5-flash
          parts.value.append(ujson.Obj(
            "inlineData" -> ujson.Obj("mimeType" -> "application/pdf", "data" -> cleanData)
          ))
        } else {
          // Imagenes: inlineData normal
          parts.value.append(ujson.Obj(
            "inlineData" -> ujson.Obj("mimeType" -> mime, "data" -> cleanData)
          ))
        }
      }

      val payload = ujson.Obj("contents" -> ujson.Arr(ujson.Obj("parts" -> parts)))

      var lastError = ""
      for (url <- urls) {
        try {
          val r = requests.post(
            url,
            data = ujson.write(payload),
            headers = Map("Content-Type" -> "application/json"),
            readTimeout = if (isVideo) 300000 else 60000  // Video necesita mucho mas tiempo; PDF un poco mas
          )
          if (r.statusCode == 200)
            return ujson.read(r.text())("candidates")(0)("content")("parts")(0)("text").str
          else {
            lastError = s"Status ${r.statusCode}: ${r.text().take(300)}"
            if (debugMode) println(s"DEBUG URL fallida: $url -> $lastError")
          }
        } catch { case e: Exception =>
          lastError = e.getMessage
          if (debugMode) println(s"DEBUG excepcion: $lastError")
        }
      }
      s"Error: $lastError"
    }
  }

  // --- EN: DatabaseManager.scala ---

  def generateTrainingSession(mode: String, focus: String): String = {
    var ctx = ""
    val conn = getConnection()
    try {
      // 1. Contexto Academico (Fase 2: Detector de Fatiga Mental)
      val rsAcad = conn.createStatement().executeQuery(
        "SELECT nota FROM academic_performance ORDER BY fecha DESC LIMIT 1"
      )
      if(rsAcad.next()) {
        val ultimaNota = rsAcad.getDouble("nota")
        // Logica de fatiga cognitiva inyectada al prompt
        if(ultimaNota < 6.0) ctx += "ESTADO COGNITIVO: Carga academica alta o estres detectado. Priorizar sesion ludica y de baja frustracion. "
        else ctx += "ESTADO COGNITIVO: Optimo. Se puede exigir alta concentracion tactica. "
      }

      // 2. Dojo Synergy (Fase 2: Judo Integration)
      val rsJudo = conn.createStatement().executeQuery("SELECT judo_belt FROM seasons ORDER BY id DESC LIMIT 1")
      if(rsJudo.next()) {
        val belt = rsJudo.getString("judo_belt")
        ctx += s"CONOCIMIENTO DOJO: Cinturon $belt. Incorporar dinamicas de caidas y agilidad de judo a la porteria. "
      }

      // 3. Alertas Tecnicas y Clima (Lo que ya teniamos de Fase 1)
      val alerts = getTechnicalAlerts()
      if(alerts.nonEmpty) ctx += s"ALERTAS TECNICAS: ${alerts.mkString(", ")}. "
    } finally {
      conn.close()
    }

    val card = getLatestCardData()
    val edad = calcularEdadExacta(card.fechaNacimiento)

    // Prompt enriquecido con Fatiga Mental y Dojo Synergy
    val prompt = s"""
    Eres Entrenador Elite. Crea una sesion de 45min (Padre/Hijo).
    ROL: PORTERO ($edad anos).
    OBJETIVO: $focus.
    CONTEXTO MULTI-DISCIPLINA: $ctx.

    ESTRUCTURA:
    1. Calentamiento (Ludico + Caidas tipo Judo).
    2. Bloque Principal (Ajustar dificultad segun ESTADO COGNITIVO).
    3. Reto Final.
    SOLO TEXTO PLANO.
  """

    AIProvider.ask(prompt).replace("```html","").replace("```","").trim
  }
  // --- NUEVO: CENTRO DE PREDICCION BIOMETRICA (EL ORACULO) ---
  def getOracleInsights(): String = {
    val conn = getConnection()
    try {
      val sb = new StringBuilder()

      // 1. Obtener datos de crecimiento (ultimos 2 para comparar)
      val rsG = conn.createStatement().executeQuery("SELECT altura, peso FROM physical_growth ORDER BY fecha DESC LIMIT 2")

      // Variables para guardar los datos y usarlos luego
      var currentH = 110.0
      var currentW = 20.0
      var hasPrev = false
      var prevH = 0.0
      var prevW = 0.0

      if (rsG.next()) {
        currentH = rsG.getDouble("altura")
        currentW = rsG.getDouble("peso")

        if (rsG.next()) {
          prevH = rsG.getDouble("altura")
          prevW = rsG.getDouble("peso")
          hasPrev = true
        }
      }

      // 2. Analisis de Biotipo y Composicion
      val imc = if(currentH > 0) currentW / Math.pow(currentH/100, 2) else 0.0
      sb.append(s"<div class='mb-3 text-white'><b>📊 COMPOSICION:</b> ${currentH}cm / ${currentW}kg</div>")

      val (perfilNombre, perfilDesc) = if (imc < 15) {
        ("<span class='text-info fw-bold'>VELOCISTA</span>", "Peso ligero que favorece la <b>agilidad pura</b> y velocidad de desplazamiento.")
      } else if (imc >= 15 && imc <= 17) {
        ("<span class='text-success fw-bold'>EQUILIBRADO</span>", "Relacion potencia-peso optima. Buen equilibrio entre <b>salto y velocidad</b>.")
      } else {
        ("<span class='text-warning fw-bold'>TANQUE</span>", "Mayor masa corporal. Ventaja en <b>proteccion de balon</b> y duelos 1v1.")
      }
      sb.append(s"<div class='mb-3 small text-light'><b>🔍 Perfil Fisico:</b> $perfilNombre. $perfilDesc</div>")

      // 3. Alerta de Estiron (Solo si hay historial)
      if (hasPrev && currentH > prevH && currentW <= prevW) {
        sb.append("<div class='alert alert-warning p-2 small mb-3'>")
        sb.append("<b>🦴 ESTIRON DETECTADO:</b> Ha crecido en altura sin aumentar masa. ")
        sb.append("Es probable que este algo mas impreciso. Trabajar <b>propiocepcion</b>.</div>")
      }

      // 4. Calculo de Cargas (ACWR) — inline para no abrir segunda conexion
      val rsAc = conn.prepareStatement("SELECT COALESCE(SUM(rpe * 60), 0) FROM trainings WHERE fecha >= CURRENT_DATE - ?")
      rsAc.setInt(1, 7); val rsAcR = rsAc.executeQuery()
      val acuteLoads = Seq(if (rsAcR.next()) rsAcR.getDouble(1) else 0.0)
      val rsCh = conn.prepareStatement("SELECT COALESCE(SUM(rpe * 60), 0) FROM trainings WHERE fecha >= CURRENT_DATE - ?")
      rsCh.setInt(1, 28); val rsChR = rsCh.executeQuery()
      val chronicLoads = Seq(if (rsChR.next()) rsChR.getDouble(1) else 0.0)
      val acuteAvg = if (acuteLoads.nonEmpty) acuteLoads.sum / 7.0 else 0.0
      val chronicAvgRaw = if (chronicLoads.nonEmpty) chronicLoads.sum / 28.0 else 1.0
      // FIX 2: mismo floor que StatsCalculator.calculateACWR — evita un ratio disparado
      val chronicAvg = Math.max(chronicAvgRaw, acuteAvg * 0.3)
      val acwr = if (chronicAvg > 0) acuteAvg / chronicAvg else 0.0

      // 5. Grafico de Barras ACWR
      val maxVal = Math.max(acuteAvg, chronicAvg).max(100.0)
      val acuteWidth = (acuteAvg / maxVal * 100).toInt
      val chronicWidth = (chronicAvg / maxVal * 100).toInt
      val barColor = if(acwr > 1.5) "bg-danger" else if(acwr < 0.8) "bg-info" else "bg-success"

      sb.append("<div class='mb-4 p-3 bg-black bg-opacity-25 rounded border border-secondary'>")
      sb.append("<h6 class='text-uppercase x-small fw-bold text-muted mb-3'>Estado de Carga (ACWR)</h6>")
      sb.append(s"<div class='mb-2'><div class='progress' style='height: 6px; background:#111;'><div class='progress-bar bg-secondary' style='width: $chronicWidth%'></div></div><div class='x-small text-muted'>Carga Cronica</div></div>")
      sb.append(s"<div class='mb-2'><div class='progress' style='height: 12px; background:#111;'><div class='progress-bar $barColor progress-bar-striped progress-bar-animated' style='width: $acuteWidth%'></div></div><div class='x-small text-muted'>Carga Aguda (Semana)</div></div>")
      sb.append(f"<div class='text-center mt-2'><span class='badge bg-dark border border-secondary'>Ratio: $acwr%.2f</span></div>")
      sb.append("</div>")

      // 6. Plan de Trabajo Dinamico
      sb.append("<div class='card bg-primary bg-opacity-10 border-primary p-3 mb-2'>")
      sb.append("<h6 class='text-primary fw-bold'><i class='fas fa-clipboard-list'></i> Plan Recomendado:</h6>")
      if (acwr > 1.5) {
        sb.append("<p class='small text-warning mb-0'><b>⚠️ FATIGA DETECTADA:</b> Sesion teorica o tecnica manual sentado.</p>")
      } else {
        sb.append("<p class='small text-light mb-0'><b>✅ LISTO:</b> Coordinacion de pies y blocajes en movimiento.</p>")
      }
      sb.append("</div>")

      sb.toString()
    } catch {
      case e: Exception => s"Analizando datos bioptometricos... (${e.getMessage})"
    } finally {
      conn.close()
    }
  }

  def getRPGStatus(): RPGStatus = {
    var xp = 0
    var belt = "Blanco"
    val conn = getConnection()

    try {
      // 1. Calculo de XP basado en rendimiento real
      val rs = conn.createStatement().executeQuery("SELECT COUNT(*) as pj, SUM(CASE WHEN goles_contra=0 THEN 1 ELSE 0 END) as cs, SUM(paradas) as sv, AVG(nota) as avg_n FROM matches WHERE status='PLAYED'")
      if(rs.next()){
        val pj = rs.getInt("pj")
        val cs = rs.getInt("cs")
        val sv = rs.getInt("sv")
        val avg = rs.getDouble("avg_n")
        xp = (pj * 50) + (cs * 100) + (sv * 5) + (if(avg > 7.0) ((avg - 7.0) * 100).toInt else 0)
      }

      // 2. Obtencion del cinturon de Judo
      val rsBelt = conn.createStatement().executeQuery("SELECT judo_belt FROM seasons ORDER BY id DESC LIMIT 1")
      if(rsBelt.next()) {
        belt = Option(rsBelt.getString("judo_belt")).getOrElse("Blanco")
      }
    } finally { conn.close() }

    // 3. Logica de progresion (Se calcula una sola vez aqui)
    val level = 1 + (xp / 1000)
    val nextLevelXp = level * 1000 // Usamos el nombre exacto de tu Case Class

    val title = level match {
      case 1 => "Novato Promesa"
      case 2 => "Portero Local"
      case 3 => "Muro Regional"
      case 4 => "Candado Nacional"
      case _ => "Leyenda Mundial"
    }

    // Retornamos el objeto con el orden y nombres correctos
    RPGStatus(level, xp, nextLevelXp, title, belt)
  }

  def getOraclePrediction(hDad: Double, hMom: Double): String = { val conn=getConnection(); try{ val rs=conn.createStatement().executeQuery("SELECT altura FROM physical_growth ORDER BY fecha DESC LIMIT 1"); val currentHeight=if(rs.next()) rs.getDouble("altura") else 115.0; val midParent=(hDad+hMom+13)/2.0; val projected=(currentHeight*(180.0/110.0)+midParent)/2.0+5.0; val minH=projected-4; val maxH=projected+4; f"<div class='text-center'><h1 class='display-1 text-warning fw-bold'>${projected.toInt} cm</h1><p class='text-muted'>Proyeccion Adulta Estimada</p><div class='progress mb-2' style='height:10px;'><div class='progress-bar bg-success' style='width:${(projected/200.0)*100}%%'></div></div><p class='small'>Rango probable: <b>${minH.toInt}cm - ${maxH.toInt}cm</b></p><hr><p class='small text-info'>Comparativa Elite: <b>189 cm</b> (Media Pro)</p></div>" } catch { case _:Exception => "Error calculando." } finally { conn.close() } }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE A — REGISTRO DE PARTIDO POR NLP (Elite exclusivamente)
  // ─────────────────────────────────────────────────────────────────────────────
  // Llamada a Gemini — SOLO desde el boton explicito "Extraer datos". Usa cache normal (ai_cache).
  def extraerPartidoNLP(texto: String): String = {
    val card = getLatestCardData()
    val edad = calcularEdadExacta(card.fechaNacimiento)
    val prompt = s"""Eres el asistente de registro de partidos de Guardian Elite. El padre de Héctor, un portero de $edad años, te describe un partido en lenguaje natural. Extrae los datos y devuelve ÚNICAMENTE un JSON válido sin backticks ni texto adicional: { rival: string, goles_favor: int, goles_contra: int, nota: float (1-10), paradas: int, clima: 'Sol'|'Nubes'|'Lluvia'|'Frio'|'Calor'|'Viento', sede: 'Casa'|'Fuera', tipo_partido: 'LIGA'|'TORNEO'|'CUP'|'AMISTOSO', iniciativa_vocal: 'SI'|'PARCIAL'|'TIMIDO'|null, rubrica_posicion: int|null (1-5), rubrica_decisiones: int|null (1-5), rubrica_pies: int|null (1-5), rubrica_comunicacion: int|null (1-5), rubrica_actitud: int|null (1-5), notas_partido: string, confianza: float (0-1) }. Si no puedes inferir un campo con confianza razonable déjalo null. Texto del padre: $texto"""

    val res = AIProvider.ask(prompt)
    res.replace("```json", "").replace("```", "").trim
  }

  // --- CORE MATCH LOGIC ---
  def logMatch(
                riv: String, gf: Int, gc: Int, min: Int, n: Double, med: Double, par: Int,
                zG: String, zT: String, zP: String, p1v1: Int, pAir: Int, pPie: Int,
                clima: String, estadio: String, temp: Int, notas: String, video: String,
                reaccion: String, fechaStr: String, tipo: String,
                pcTot: Int, pcOk: Int, plTot: Int, plOk: Int,
                mapaCampo: String,
                lineasSup: Int = 0, scanningRate: Int = 0, esLocal: Option[Boolean] = None,
                comportamientoPresion: String = "", nutricionPrepartido: String = "",
                cornersDominados: Int = 0, cornersCedidos: Int = 0, faltasAreaDominadas: Int = 0
              ): Int = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("SELECT MAX(id) as id FROM seasons")
      if(rs.next()){
        // BLOQUE E: RETURNING id evita la condicion de carrera de un SELECT MAX(id) posterior
        val s = conn.prepareStatement("""
        INSERT INTO matches (
          season_id, rival, goles_favor, goles_contra, minutos, nota, media_historica,
          paradas, zona_goles, zona_tiros, zona_paradas, paradas_1v1, paradas_aereas,
          acciones_pie, clima, estadio, temperatura, notas_partido, video_url,
          reaccion_goles, fecha, status, tipo_partido, pc_t, pc_ok, pl_t, pl_ok,
          torneo_nombre, fase, mapa_campo, lineas_superadas, scanning_rate, es_local,
          corners_dominados, corners_cedidos, faltas_area_dominadas
        ) VALUES (
          ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?,
          'PLAYED', ?, ?, ?, ?, ?, '', '', ?, ?, ?, ?, ?, ?, ?
        ) RETURNING id
      """)
        s.setInt(1, rs.getInt("id"))
        s.setString(2, fixEncoding(riv))
        s.setInt(3, gf)
        s.setInt(4, gc)
        s.setInt(5, min)
        s.setDouble(6, n)
        s.setDouble(7, med)
        s.setInt(8, par)
        s.setString(9, zG)
        s.setString(10, zT)
        s.setString(11, zP)
        s.setInt(12, p1v1)
        s.setInt(13, pAir)
        s.setInt(14, pPie)
        s.setString(15, clima)
        s.setString(16, fixEncoding(estadio))
        s.setInt(17, temp)
        s.setString(18, fixEncoding(notas))
        s.setString(19, video)
        s.setString(20, fixEncoding(reaccion))
        s.setDate(21, Date.valueOf(fechaStr))
        s.setString(22, tipo)
        s.setInt(23, pcTot)
        s.setInt(24, pcOk)
        s.setInt(25, plTot)
        s.setInt(26, plOk)
        s.setString(27, mapaCampo)
        s.setInt(28, lineasSup)
        s.setInt(29, scanningRate)
        esLocal match {
          case Some(v) => s.setBoolean(30, v)
          case None    => s.setNull(30, java.sql.Types.BOOLEAN)
        }
        s.setInt(31, cornersDominados)
        s.setInt(32, cornersCedidos)
        s.setInt(33, faltasAreaDominadas)
        val idRs = s.executeQuery()
        val newMatchId = if (idRs.next()) idRs.getInt("id") else 0
        if ((comportamientoPresion.nonEmpty && comportamientoPresion != "NA") || nutricionPrepartido.nonEmpty) {
          if (comportamientoPresion.nonEmpty && comportamientoPresion != "NA") {
            val ps2 = conn.prepareStatement("UPDATE matches SET comportamiento_presion = ? WHERE id = ?")
            ps2.setString(1, comportamientoPresion); ps2.setInt(2, newMatchId)
            ps2.executeUpdate()
          }
          if (nutricionPrepartido.nonEmpty) {
            val ps3 = conn.prepareStatement("UPDATE matches SET nutricion_prepartido = ? WHERE id = ?")
            ps3.setString(1, nutricionPrepartido); ps3.setInt(2, newMatchId)
            ps3.executeUpdate()
          }
        }
        newMatchId
      } else 0
    } finally {
      conn.close()
    }
  }
  // --- FOOTBAR (SENSOR GPS DE RENDIMIENTO FISICO/TECNICO) ---
  def saveFootbar(
    matchId: Int, distanciaKm: Double, altaIntensidadM: Double, sprintMaxKmh: Double,
    pctActividad: Double, tiempoActividadMin: Int, aceleraciones: Int, desaceleraciones: Int,
    balones: Int, pases: Int, tiempoBalonSeg: Int, disparos: Int, tiroMaxKmh: Double
  ): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("""
        INSERT INTO footbar_sessions
          (match_id, distancia_km, alta_intensidad_m, sprint_max_kmh, pct_actividad,
           tiempo_actividad_min, aceleraciones, desaceleraciones, balones, pases,
           tiempo_balon_seg, disparos, tiro_max_kmh)
        VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)
        ON CONFLICT (match_id) DO UPDATE SET
          distancia_km = EXCLUDED.distancia_km, alta_intensidad_m = EXCLUDED.alta_intensidad_m,
          sprint_max_kmh = EXCLUDED.sprint_max_kmh, pct_actividad = EXCLUDED.pct_actividad,
          tiempo_actividad_min = EXCLUDED.tiempo_actividad_min, aceleraciones = EXCLUDED.aceleraciones,
          desaceleraciones = EXCLUDED.desaceleraciones, balones = EXCLUDED.balones, pases = EXCLUDED.pases,
          tiempo_balon_seg = EXCLUDED.tiempo_balon_seg, disparos = EXCLUDED.disparos, tiro_max_kmh = EXCLUDED.tiro_max_kmh
      """)
      ps.setInt(1, matchId)
      ps.setDouble(2, distanciaKm)
      ps.setDouble(3, altaIntensidadM)
      ps.setDouble(4, sprintMaxKmh)
      ps.setDouble(5, pctActividad)
      ps.setInt(6, tiempoActividadMin)
      ps.setInt(7, aceleraciones)
      ps.setInt(8, desaceleraciones)
      ps.setInt(9, balones)
      ps.setInt(10, pases)
      ps.setInt(11, tiempoBalonSeg)
      ps.setInt(12, disparos)
      ps.setDouble(13, tiroMaxKmh)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def getFootbar(matchId: Int): Option[FootbarSession] = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("SELECT * FROM footbar_sessions WHERE match_id = ?")
      ps.setInt(1, matchId)
      val rs = ps.executeQuery()
      if (rs.next()) Some(FootbarSession(
        matchId, rs.getDouble("distancia_km"), rs.getDouble("alta_intensidad_m"), rs.getDouble("sprint_max_kmh"),
        rs.getDouble("pct_actividad"), rs.getInt("tiempo_actividad_min"), rs.getInt("aceleraciones"), rs.getInt("desaceleraciones"),
        rs.getInt("balones"), rs.getInt("pases"), rs.getInt("tiempo_balon_seg"), rs.getInt("disparos"), rs.getDouble("tiro_max_kmh")
      )) else None
    } finally { conn.close() }
  }

  // Correlacion Pearson entre distancia recorrida (Footbar) y nota del partido
  def getFootbarCorrelacion(): Double = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("""
        SELECT f.distancia_km, m.nota FROM footbar_sessions f
        JOIN matches m ON m.id = f.match_id
        WHERE f.distancia_km > 0
      """)
      var pairs = List[(Double, Double)]()
      while (rs.next()) pairs = pairs :+ (rs.getDouble("distancia_km"), rs.getDouble("nota"))
      calcCorrelation(pairs)
    } finally { conn.close() }
  }

  // Datos para la pagina /footbar: KPIs medios, tabla por partido y serie distancia/nota
  def getFootbarPageData(): Map[String, Any] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("""
        SELECT f.*, m.rival, m.fecha, m.nota
        FROM footbar_sessions f
        JOIN matches m ON m.id = f.match_id
        WHERE m.status='PLAYED'
        ORDER BY m.fecha DESC
      """)
      var rows = List[Map[String, Any]]()
      while (rs.next()) {
        rows = rows :+ Map(
          "matchId"        -> rs.getInt("match_id"),
          "rival"          -> Option(rs.getString("rival")).getOrElse(""),
          "fecha"          -> rs.getDate("fecha").toString,
          "nota"           -> rs.getDouble("nota"),
          "distanciaKm"    -> rs.getDouble("distancia_km"),
          "sprintMaxKmh"   -> rs.getDouble("sprint_max_kmh"),
          "pases"          -> rs.getInt("pases"),
          "disparos"       -> rs.getInt("disparos")
        )
      }

      def avg(f: Map[String, Any] => Double): Double =
        if (rows.isEmpty) 0.0 else rows.map(f).sum / rows.size

      // BLOQUE B: sesiones de entrenamiento con datos Footbar (fb_distancia)
      val rsT = conn.createStatement().executeQuery("""
        SELECT fecha, tipo, foco, rpe, calidad, fb_distancia, fb_sprint_max, fb_pct_actividad,
               fb_aceleraciones, fb_desaceleraciones
        FROM trainings
        WHERE fb_distancia IS NOT NULL
        ORDER BY fecha DESC
      """)
      var trainingRows = List[Map[String, Any]]()
      while (rsT.next()) {
        trainingRows = trainingRows :+ Map(
          "fecha"        -> rsT.getDate("fecha").toString,
          "tipo"         -> Option(rsT.getString("tipo")).getOrElse(""),
          "foco"         -> Option(rsT.getString("foco")).getOrElse(""),
          "rpe"          -> rsT.getInt("rpe"),
          "calidad"      -> rsT.getInt("calidad"),
          "distanciaKm"  -> rsT.getDouble("fb_distancia"),
          "sprintMaxKmh" -> rsT.getDouble("fb_sprint_max"),
          "pctActividad" -> rsT.getInt("fb_pct_actividad"),
          "aceleraciones"    -> rsT.getInt("fb_aceleraciones"),
          "desaceleraciones" -> rsT.getInt("fb_desaceleraciones")
        )
      }

      Map(
        "rows"            -> rows,
        "trainingRows"    -> trainingRows,
        "totalSesiones"   -> rows.size,
        "avgDistanciaKm"  -> avg(_("distanciaKm").asInstanceOf[Double]),
        "maxSprintKmh"    -> (if (rows.isEmpty) 0.0 else rows.map(_("sprintMaxKmh").asInstanceOf[Double]).max),
        "avgPases"        -> avg(_("pases").asInstanceOf[Int].toDouble),
        "correlacionNota" -> getFootbarCorrelacion()
      )
    } finally { conn.close() }
  }

  // ── MODULO 2: CHECKLIST DE HABILIDADES DE PORTERO ───────────────────────
  def getGoalkeeperSkills(): List[GoalkeeperSkill] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("SELECT * FROM goalkeeper_skills ORDER BY id ASC")
      var list = List[GoalkeeperSkill]()
      while (rs.next()) {
        list = list :+ GoalkeeperSkill(
          rs.getInt("id"), rs.getString("categoria"), rs.getString("habilidad"),
          rs.getBoolean("conseguido"),
          Option(rs.getDate("fecha_conseguido")).map(_.toString),
          Option(rs.getString("contexto_conseguido")),
          Option(rs.getString("notas")).getOrElse(""),
          Option(rs.getDate("fecha_inicio_trabajo")).map(_.toString),
          Option(rs.getString("nivel_automatismo"))
        )
      }
      list
    } finally { conn.close() }
  }

  // BLOQUE N: actualiza el nivel de automatismo de una habilidad ya conseguida
  def setNivelAutomatismo(id: Int, nivel: String): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("UPDATE goalkeeper_skills SET nivel_automatismo = ? WHERE id = ? AND conseguido = TRUE")
      if (nivel.nonEmpty) ps.setString(1, nivel) else ps.setNull(1, java.sql.Types.VARCHAR)
      ps.setInt(2, id)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  /** Desglose por nivel de automatismo de las habilidades conseguidas — para getDeepAnalysis(). */
  def getAutomatismoBreakdown(): Map[String, Int] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT nivel_automatismo, COUNT(*) as n FROM goalkeeper_skills WHERE conseguido = TRUE GROUP BY nivel_automatismo")
      var instintivas = 0; var automaticas = 0; var conscientes = 0
      while (rs.next()) {
        val n = rs.getInt("n")
        Option(rs.getString("nivel_automatismo")) match {
          case Some("INSTINTIVO") => instintivas = n
          case Some("AUTOMATICO") => automaticas = n
          case Some("CONSCIENTE") => conscientes = n
          case _ => ()
        }
      }
      Map("instintivas" -> instintivas, "automaticas" -> automaticas, "conscientes" -> conscientes)
    } finally { conn.close() }
  }

  def setSkillAchieved(id: Int, achieved: Boolean, contexto: String, fechaInicioTrabajo: String = ""): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement(
        "UPDATE goalkeeper_skills SET conseguido = ?, fecha_conseguido = ?, contexto_conseguido = ? WHERE id = ?"
      )
      ps.setBoolean(1, achieved)
      if (achieved) {
        ps.setDate(2, Date.valueOf(LocalDate.now()))
        ps.setString(3, contexto)
      } else {
        ps.setNull(2, java.sql.Types.DATE)
        ps.setNull(3, java.sql.Types.VARCHAR)
      }
      ps.setInt(4, id)
      ps.executeUpdate()

      // Solo se guarda la primera vez (no sobreescribe un fecha_inicio_trabajo ya registrado)
      if (achieved && fechaInicioTrabajo.nonEmpty) {
        val ps2 = conn.prepareStatement(
          "UPDATE goalkeeper_skills SET fecha_inicio_trabajo = ?::date WHERE id = ? AND fecha_inicio_trabajo IS NULL"
        )
        ps2.setString(1, fechaInicioTrabajo); ps2.setInt(2, id)
        ps2.executeUpdate()
      }
    } finally { conn.close() }
  }

  def updateSkillNotes(id: Int, notas: String): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("UPDATE goalkeeper_skills SET notas = ? WHERE id = ?")
      ps.setString(1, fixEncoding(notas))
      ps.setInt(2, id)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // MODULO 2 — INDICE DE VELOCIDAD DE APRENDIZAJE
  // ─────────────────────────────────────────────────────────────────────────────
  def getLearningVelocityIndex(): Map[String, Any] = {
    val conn = getConnection()
    try {
      val rsCache = conn.prepareStatement(
        "SELECT payload FROM feature_cache WHERE cache_key = 'learning_velocity' AND updated_at > NOW() - INTERVAL '7 days'"
      ).executeQuery()
      if (rsCache.next()) {
        val json = ujson.read(rsCache.getString("payload"))
        return Map(
          "suficiente" -> json("suficiente").bool,
          "indice"     -> json("indice").num,
          "mediaDias"  -> json("mediaDias").num,
          "porCategoria" -> json("porCategoria").arr.map(c => Map(
            "categoria" -> c("categoria").str, "mediaDias" -> c("mediaDias").num
          )).toList,
          "porHabilidad" -> json("porHabilidad").arr.map(h => Map(
            "habilidad" -> h("habilidad").str, "dias" -> h("dias").num.toInt
          )).toList,
          "analisisIA" -> json("analisisIA").str
        )
      }

      case class SkillDuration(habilidad: String, categoria: String, dias: Int)
      val rs = conn.createStatement().executeQuery("""
        SELECT habilidad, categoria, fecha_inicio_trabajo, fecha_conseguido
        FROM goalkeeper_skills
        WHERE conseguido = TRUE AND fecha_inicio_trabajo IS NOT NULL AND fecha_conseguido IS NOT NULL
        ORDER BY fecha_conseguido ASC
      """)
      var durations = List[SkillDuration]()
      while (rs.next()) {
        val inicio = rs.getDate("fecha_inicio_trabajo").toLocalDate
        val fin = rs.getDate("fecha_conseguido").toLocalDate
        val dias = math.max(1, java.time.temporal.ChronoUnit.DAYS.between(inicio, fin).toInt)
        durations = durations :+ SkillDuration(fixEncoding(rs.getString("habilidad")), rs.getString("categoria"), dias)
      }

      if (durations.size < 3) {
        return Map("suficiente" -> false, "indice" -> 0.0, "mediaDias" -> 0.0,
          "porCategoria" -> List.empty[Map[String, Any]], "porHabilidad" -> List.empty[Map[String, Any]],
          "analisisIA" -> "")
      }

      val mediaDias = durations.map(_.dias).sum.toDouble / durations.size
      val indice = 100.0 / (mediaDias / 30.0)

      val porCategoria = durations.groupBy(_.categoria).map { case (cat, ds) =>
        Map("categoria" -> cat, "mediaDias" -> (ds.map(_.dias).sum.toDouble / ds.size))
      }.toList

      val porHabilidad = durations.sortBy(_.dias).map(d => Map("habilidad" -> d.habilidad, "dias" -> d.dias))

      val card = getLatestCardData()
      val edad = calcularEdadExacta(card.fechaNacimiento)
      val categoriasStr = porCategoria.map(c => s"${c("categoria")}=${f"${c("mediaDias").asInstanceOf[Double]}%.0f"}días").mkString(", ")

      val prompt = s"""Héctor, portero de $edad años, tarda en media ${f"$mediaDias%.0f"} días en consolidar una habilidad nueva. Por categorías: $categoriasStr. Según la metodología de detección de talento en fútbol base, ¿qué indica esta velocidad de aprendizaje sobre su potencial de desarrollo? Dame: 1) Evaluación del índice (lento/normal/rápido para su edad), 2) La categoría donde aprende más rápido y qué implica, 3) Una recomendación para el entrenador de academia. Máximo 3 líneas por punto."""

      val analisisIA = AIProvider.ask(prompt)

      val payload = ujson.Obj(
        "suficiente" -> true, "indice" -> indice, "mediaDias" -> mediaDias,
        "porCategoria" -> ujson.Arr(porCategoria.map(c => ujson.Obj(
          "categoria" -> c("categoria").asInstanceOf[String], "mediaDias" -> c("mediaDias").asInstanceOf[Double]
        ): ujson.Value): _*),
        "porHabilidad" -> ujson.Arr(porHabilidad.map(h => ujson.Obj(
          "habilidad" -> h("habilidad").asInstanceOf[String], "dias" -> h("dias").asInstanceOf[Int]
        ): ujson.Value): _*),
        "analisisIA" -> analisisIA
      )
      val upsert = conn.prepareStatement("""
        INSERT INTO feature_cache (cache_key, payload, updated_at) VALUES ('learning_velocity', ?, NOW())
        ON CONFLICT (cache_key) DO UPDATE SET payload = EXCLUDED.payload, updated_at = NOW()
      """)
      upsert.setString(1, ujson.write(payload))
      upsert.executeUpdate()

      Map("suficiente" -> true, "indice" -> indice, "mediaDias" -> mediaDias,
        "porCategoria" -> porCategoria, "porHabilidad" -> porHabilidad, "analisisIA" -> analisisIA)
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // MODULO 3 (sesion actual) — COMPORTAMIENTO BAJO PRESION
  // ─────────────────────────────────────────────────────────────────────────────
  private val presionLabels: Map[String, String] = Map(
    "RAPIDO"   -> "✅ Se repuso rápido",
    "LIDER"    -> "💪 Lideró al equipo",
    "NEUTRO"   -> "😐 Neutro",
    "AFECTADO" -> "😟 Se afectó visiblemente",
    "INTENSO"  -> "🔥 Reaccionó con más intensidad"
  )

  // Solo estadistica (sin Gemini) — seguro de llamar en el render de pagina
  def getPresionPattern(): Map[String, Any] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("""
        SELECT comportamiento_presion, COUNT(*) as cnt
        FROM matches
        WHERE status='PLAYED' AND goles_contra > 0
          AND comportamiento_presion IS NOT NULL AND comportamiento_presion <> '' AND comportamiento_presion <> 'NA'
        GROUP BY comportamiento_presion
        ORDER BY cnt DESC
      """)
      var counts = List[(String, Int)]()
      while (rs.next()) counts = counts :+ (rs.getString("comportamiento_presion"), rs.getInt("cnt"))
      val total = counts.map(_._2).sum

      if (total == 0)
        return Map("total" -> 0, "distribucion" -> List.empty[Map[String, Any]], "masFrecuente" -> "", "analisisIA" -> Option.empty[String])

      val distribucion = counts.map { case (tipo, cnt) =>
        Map[String, Any]("tipo" -> tipo, "label" -> presionLabels.getOrElse(tipo, tipo), "count" -> cnt, "pct" -> (cnt * 100 / total))
      }
      val masFrecuente = presionLabels.getOrElse(counts.head._1, counts.head._1)

      val rsCache = conn.prepareStatement(
        "SELECT payload FROM feature_cache WHERE cache_key = 'presion_pattern' AND updated_at > NOW() - INTERVAL '30 days'"
      ).executeQuery()
      val analisisIA: Option[String] = if (rsCache.next()) Some(ujson.read(rsCache.getString("payload"))("analisisIA").str) else None

      Map("total" -> total, "distribucion" -> distribucion, "masFrecuente" -> masFrecuente, "analisisIA" -> analisisIA)
    } finally { conn.close() }
  }

  // Llamada real a Gemini — SOLO se invoca desde el boton POST "Analisis IA" (min 5 registros)
  def generatePresionAnalysis(): String = {
    val conn = getConnection()
    try {
      val d = getPresionPattern()
      val total = d("total").asInstanceOf[Int]
      if (total < 5) return "Registra al menos 5 partidos con goles encajados para generar el análisis."
      val distribucion = d("distribucion").asInstanceOf[List[Map[String, Any]]]

      val card = getLatestCardData()
      val edad = calcularEdadExacta(card.fechaNacimiento)
      val distribucionStr = distribucion.map(x => s"${x("label")}: ${x("pct")}%").mkString(", ")

      val prompt = s"""Héctor, portero de $edad años. Cuando encaja goles en partido, su comportamiento registrado es: $distribucionStr. Analiza su perfil psicológico bajo presión en 3 líneas. ¿Es una señal positiva o requiere trabajo específico?"""
      val analisisIA = AIProvider.ask(prompt, None, bypassCache = true)

      val payload = ujson.Obj("analisisIA" -> analisisIA)
      val upsert = conn.prepareStatement("""
        INSERT INTO feature_cache (cache_key, payload, updated_at) VALUES ('presion_pattern', ?, NOW())
        ON CONFLICT (cache_key) DO UPDATE SET payload = EXCLUDED.payload, updated_at = NOW()
      """)
      upsert.setString(1, ujson.write(payload))
      upsert.executeUpdate()
      analisisIA
    } finally { conn.close() }
  }

  // ── MODULO 3: VISIBILIDAD Y OPORTUNIDADES ───────────────────────────────
  def getOpportunities(): List[Opportunity] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("SELECT * FROM opportunities ORDER BY fecha DESC, id DESC")
      var list = List[Opportunity]()
      while (rs.next()) {
        val cid = rs.getInt("contact_id")
        val contactIdOpt = if (rs.wasNull()) None else Some(cid)
        list = list :+ Opportunity(
          rs.getInt("id"), rs.getDate("fecha").toString, rs.getString("tipo"),
          Option(rs.getString("descripcion")).getOrElse(""),
          Option(rs.getString("club_o_entidad")).getOrElse(""),
          Option(rs.getString("resultado")).getOrElse(""),
          Option(rs.getString("seguimiento")).getOrElse(""),
          rs.getBoolean("seguimiento_completado"),
          contactIdOpt
        )
      }
      list
    } finally { conn.close() }
  }

  def getOpportunitiesByContact(contactId: Int): List[Opportunity] =
    getOpportunities().filter(_.contactId.contains(contactId))

  def saveOpportunity(fecha: String, tipo: String, descripcion: String, clubOEntidad: String,
                       resultado: String, seguimiento: String, contactId: Option[Int] = None): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("""
        INSERT INTO opportunities (fecha, tipo, descripcion, club_o_entidad, resultado, seguimiento, contact_id)
        VALUES (?, ?, ?, ?, ?, ?, ?)
      """)
      ps.setDate(1, Date.valueOf(if (fecha.nonEmpty) fecha else LocalDate.now().toString))
      ps.setString(2, tipo)
      ps.setString(3, fixEncoding(descripcion))
      ps.setString(4, fixEncoding(clubOEntidad))
      ps.setString(5, fixEncoding(resultado))
      ps.setString(6, fixEncoding(seguimiento))
      contactId match {
        case Some(cid) => ps.setInt(7, cid)
        case None      => ps.setNull(7, java.sql.Types.INTEGER)
      }
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def updateOpportunityResultado(id: Int, resultado: String): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("UPDATE opportunities SET resultado = ? WHERE id = ?")
      ps.setString(1, fixEncoding(resultado))
      ps.setInt(2, id)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def completeSeguimiento(id: Int): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("UPDATE opportunities SET seguimiento_completado = TRUE WHERE id = ?")
      ps.setInt(1, id)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  // ── MODULO 4: INDICE DE RESILIENCIA MENTAL ──────────────────────────────
  def getResilienceIndex(): Map[String, Any] = {
    val conn = getConnection()
    try {
      // Cache: 7 dias
      val rsCache = conn.prepareStatement(
        "SELECT payload FROM feature_cache WHERE cache_key = 'resilience_index' AND updated_at > NOW() - INTERVAL '7 days'"
      ).executeQuery()
      if (rsCache.next()) {
        val json = ujson.read(rsCache.getString("payload"))
        return Map(
          "indice"        -> json("indice").num,
          "perfil"        -> json("perfil").str,
          "recomendacion" -> json("recomendacion").str,
          "eventos"       -> json("eventos").arr.map(e => Map(
            "label" -> e("label").str, "antes" -> e("antes").num, "despues" -> e("despues").num
          )).toList
        )
      }

      case class M(fecha: String, nota: Double, gc: Int)
      val rsMatches = conn.createStatement().executeQuery(
        "SELECT fecha, nota, goles_contra FROM matches WHERE status='PLAYED' ORDER BY fecha ASC"
      )
      var all = List[M]()
      while (rsMatches.next())
        all = all :+ M(rsMatches.getString("fecha"), rsMatches.getDouble("nota"), rsMatches.getInt("goles_contra"))

      // Post-goleada (GC >= 3): nota del partido siguiente
      var postGoleada = List[Double]()
      var eventos = List[(String, Double, Double)]() // (label, antes, despues)
      for (i <- all.indices) {
        if (all(i).gc >= 3 && i + 1 < all.size) {
          postGoleada = postGoleada :+ all(i + 1).nota
          eventos = eventos :+ ((s"GC${all(i).gc} (${all(i).fecha.take(10)})", all(i).nota, all(i + 1).nota))
        }
      }

      // Post-lesion: primer partido con fecha posterior al alta de cada lesion
      val rsInjuries = conn.createStatement().executeQuery(
        "SELECT fecha_alta FROM injuries WHERE fecha_alta IS NOT NULL ORDER BY fecha_alta ASC"
      )
      var fechasAlta = List[String]()
      while (rsInjuries.next()) fechasAlta = fechasAlta :+ rsInjuries.getString("fecha_alta")

      var postLesion = List[Double]()
      fechasAlta.foreach { fAlta =>
        val despuesOpt = all.find(_.fecha > fAlta)
        val antesOpt   = all.filter(_.fecha < fAlta).lastOption
        despuesOpt.foreach { d =>
          postLesion = postLesion :+ d.nota
          eventos = eventos :+ ((s"Vuelta lesión (${fAlta.take(10)})", antesOpt.map(_.nota).getOrElse(0.0), d.nota))
        }
      }

      if (postGoleada.isEmpty && postLesion.isEmpty) {
        return Map("indice" -> 0.0, "perfil" -> "Sin datos suficientes todavía", "recomendacion" -> "", "eventos" -> List.empty[Map[String, Any]])
      }

      val card = getLatestCardData()
      val edad = calcularEdadExacta(card.fechaNacimiento)

      val prompt = s"""Eres psicólogo deportivo especializado en fútbol base. Héctor es un portero de $edad años.
Tras partidos con 3 o más goles encajados, sus notas siguientes fueron: ${postGoleada.mkString(", ")}.
Tras lesiones, sus notas de vuelta fueron: ${postLesion.mkString(", ")}.
Analiza su resiliencia mental. Dame:
1) Un índice numérico del 1 al 10
2) Una frase de máximo 15 palabras describiendo su perfil mental
3) Una recomendación concreta para el padre
Responde en texto plano con exactamente este formato (sin markdown):
INDICE: <numero>
PERFIL: <frase>
RECOMENDACION: <texto>"""

      val respuesta = AIProvider.ask(prompt)

      def extract(tag: String): String = {
        val regex = s"(?i)$tag:\\s*(.+)".r
        regex.findFirstMatchIn(respuesta).map(_.group(1).trim).getOrElse("")
      }
      val indice = extract("INDICE").takeWhile(c => c.isDigit || c == '.').toDoubleOption.getOrElse(5.0)
      val perfil = extract("PERFIL")
      val recomendacion = extract("RECOMENDACION")

      val eventosJson = ujson.Arr(eventos.map { case (label, antes, despues) =>
        ujson.Obj("label" -> label, "antes" -> antes, "despues" -> despues): ujson.Value
      }: _*)
      val payload = ujson.Obj("indice" -> indice, "perfil" -> perfil, "recomendacion" -> recomendacion, "eventos" -> eventosJson)

      val upsert = conn.prepareStatement("""
        INSERT INTO feature_cache (cache_key, payload, updated_at) VALUES ('resilience_index', ?, NOW())
        ON CONFLICT (cache_key) DO UPDATE SET payload = EXCLUDED.payload, updated_at = NOW()
      """)
      upsert.setString(1, ujson.write(payload))
      upsert.executeUpdate()

      Map(
        "indice" -> indice, "perfil" -> perfil, "recomendacion" -> recomendacion,
        "eventos" -> eventos.map { case (l, a, d) => Map("label" -> l, "antes" -> a, "despues" -> d) }
      )
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // MODULO 1 (RAE) — CORRECTOR DE EDAD RELATIVA
  // ─────────────────────────────────────────────────────────────────────────────
  // Factor RAE por trimestre de nacimiento. Hector nacio el 19 de junio (T2) -> 1.06.
  def raeFactorForMonth(month: Int): Double = month match {
    case 1 | 2 | 3  => 1.00
    case 4 | 5 | 6  => 1.06
    case 7 | 8 | 9  => 1.12
    case _          => 1.20
  }

  def getRaeAdjustedStats(): Map[String, Any] = {
    val card = getLatestCardData()
    val birthMonth = try LocalDate.parse(card.fechaNacimiento).getMonthValue catch { case _: Exception => 6 }
    val raeFactor = raeFactorForMonth(birthMonth)
    val matches = getMatchesList()
    val pj = matches.size
    val notaMedia = if (pj > 0) matches.map(_.nota).sum / pj else 0.0
    def gcOf(m: MatchLog): Int = m.resultado.split("-").lastOption.flatMap(_.trim.toIntOption).getOrElse(1)
    def gfOf(m: MatchLog): Int = m.resultado.split("-").headOption.flatMap(_.trim.toIntOption).getOrElse(0)
    val cleanSheets = matches.count(gcOf(_) == 0)
    val pctCS = if (pj > 0) cleanSheets * 100 / pj else 0
    val ganados = matches.count(m => gfOf(m) > gcOf(m))
    val winRate = if (pj > 0) ganados * 100 / pj else 0

    Map(
      "raeFactor"     -> raeFactor,
      "pj"            -> pj,
      "notaMediaReal" -> notaMedia,
      "notaMediaRae"  -> math.min(10.0, notaMedia * raeFactor),
      "pctCSReal"     -> pctCS,
      "pctCSRae"      -> math.min(100, (pctCS * raeFactor).toInt),
      "winRateReal"   -> winRate,
      "winRateRae"    -> math.min(100, (winRate * raeFactor).toInt)
    )
  }

  // ── MODULO 5: BENCHMARKING CONTRA PORTEROS DE SU EDAD ───────────────────
  // BLOQUE B3: seasonId=0 = historico completo (comportamiento anterior, sin cambios)
  def getBenchmark(seasonId: Int = 0): Map[String, Any] = {
    val conn = getConnection()
    try {
      val rae = getRaeAdjustedStats()
      // BLOQUE A4 (RFMF): percentil real contra la categoria — complementa, nunca reemplaza, el analisis de Gemini
      val rffmReal: Map[String, Any] = Map("rffmReal" -> getPercentilRealHector(seasonId))
      val cacheKey = if (seasonId > 0) s"benchmark_$seasonId" else "benchmark"

      val rsCacheStmt = conn.prepareStatement(
        "SELECT payload FROM feature_cache WHERE cache_key = ? AND updated_at > NOW() - INTERVAL '7 days'")
      rsCacheStmt.setString(1, cacheKey)
      val rsCache = rsCacheStmt.executeQuery()
      if (rsCache.next()) {
        val json = ujson.read(rsCache.getString("payload"))
        return Map("percentil" -> json("percentil").str, "areas" -> json("areas").str,
                   "referencia" -> json("referencia").str, "sinDatos" -> false) ++ rae ++ rffmReal
      }

      val card = getLatestCardData()
      val edad = calcularEdadExacta(card.fechaNacimiento)
      val matches = getMatchesList(seasonId) // ORDER BY fecha DESC
      val pj = matches.size

      if (pj < 3) return Map("percentil" -> "", "areas" -> "", "referencia" -> "", "sinDatos" -> true) ++ rae ++ rffmReal

      val notaMedia = matches.map(_.nota).sum / pj
      def gcOf(m: MatchLog): Int = m.resultado.split("-").lastOption.flatMap(_.trim.toIntOption).getOrElse(1)
      val cleanSheets = matches.count(gcOf(_) == 0)
      val pctCS = cleanSheets * 100 / pj
      val acute = getWorkloads(7); val chronic = getWorkloads(28)
      val acwr = StatsCalculator.calculateACWR(acute, chronic)
      val racha = matches.takeWhile(gcOf(_) == 0).size
      val raeFactor = rae("raeFactor").asInstanceOf[Double]

      val prompt = s"""Héctor tiene $edad años, es portero de fútbol base español. Sus estadísticas actuales: nota media ${f"$notaMedia%.1f"}, clean sheets $cleanSheets de $pj partidos ($pctCS%), ACWR ${f"$acwr%.2f"}, racha actual $racha partidos.
IMPORTANTE: Héctor nació en junio (segundo trimestre). Aplica el Efecto de Edad Relativa en tu análisis — sus métricas reales deben interpretarse con un factor de ajuste de ${f"$raeFactor%.2f"} respecto a jugadores nacidos en enero-marzo. Esto significa que su rendimiento real es un ${((raeFactor - 1.0) * 100).toInt}% más alto de lo que muestran los números brutos.
Entrena 2 días con su equipo, 1 día de academia específica de porteros semanalmente, y 2 días de judo.
Basándote en perfiles públicos de porteros que llegaron a academias de Primera División española con 8-10 años:
1) Percentil estimado de progresión (0-100) con justificación breve
2) Las 2 áreas que debería priorizar para mejorar su proyección
3) Una referencia real de portero español que empezó con un perfil similar a esta edad
Responde en texto plano, máximo 4 líneas por punto, con exactamente este formato:
PERCENTIL: <texto>
AREAS: <texto>
REFERENCIA: <texto>"""

      val respuesta = AIProvider.ask(prompt)

      def extractSection(resp: String, tag: String, nextTag: Option[String]): String = {
        val upper = resp.toUpperCase
        val startIdx = upper.indexOf(s"$tag:")
        if (startIdx < 0) return ""
        val contentStart = startIdx + tag.length + 1
        val endIdx = nextTag.map(nt => upper.indexOf(s"$nt:", contentStart)).filter(_ >= 0).getOrElse(resp.length)
        resp.substring(contentStart, endIdx).trim
      }
      val percentilTxt  = extractSection(respuesta, "PERCENTIL", Some("AREAS"))
      val areasTxt      = extractSection(respuesta, "AREAS", Some("REFERENCIA"))
      val referenciaTxt = extractSection(respuesta, "REFERENCIA", None)

      val payload = ujson.Obj("percentil" -> percentilTxt, "areas" -> areasTxt, "referencia" -> referenciaTxt)
      val upsert = conn.prepareStatement("""
        INSERT INTO feature_cache (cache_key, payload, updated_at) VALUES (?, ?, NOW())
        ON CONFLICT (cache_key) DO UPDATE SET payload = EXCLUDED.payload, updated_at = NOW()
      """)
      upsert.setString(1, cacheKey)
      upsert.setString(2, ujson.write(payload))
      upsert.executeUpdate()

      Map("percentil" -> percentilTxt, "areas" -> areasTxt, "referencia" -> referenciaTxt, "sinDatos" -> false) ++ rae ++ rffmReal
    } finally { conn.close() }
  }

  // BLOQUE B3: invalida el cache de la temporada indicada (o el global si seasonId=0)
  def invalidateBenchmarkCache(seasonId: Int = 0): Unit = {
    val cacheKey = if (seasonId > 0) s"benchmark_$seasonId" else "benchmark"
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("DELETE FROM feature_cache WHERE cache_key = ?")
      ps.setString(1, cacheKey)
      ps.executeUpdate()
    }
    finally { conn.close() }
  }

  // ── MODULO 6: PERIODIZACION ANUAL ───────────────────────────────────────
  def periodizationTipoColor(tipo: String): String = tipo match {
    case "CARGA_ALTA"          => "#dc3545"
    case "DESCARGA"            => "#20c997"
    case "TORNEO_CLAVE"        => "#ffc107"
    case "VENTANA_ACADEMIAS"   => "#0dcaf0"
    case "EVALUACION"          => "#8b5cf6"
    case "DESCANSO"            => "#6c757d"
    case _                     => "#6c757d"
  }

  def getPeriodization(): List[PeriodizationBlock] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("SELECT * FROM periodization ORDER BY fecha_inicio ASC")
      var list = List[PeriodizationBlock]()
      while (rs.next()) {
        list = list :+ PeriodizationBlock(
          rs.getInt("id"), rs.getString("nombre"), rs.getDate("fecha_inicio").toString,
          rs.getDate("fecha_fin").toString, rs.getString("tipo"),
          Option(rs.getString("notas")).getOrElse(""), Option(rs.getString("color")).getOrElse("#6c757d")
        )
      }
      list
    } finally { conn.close() }
  }

  def savePeriodization(nombre: String, fechaInicio: String, fechaFin: String, tipo: String, notas: String): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement(
        "INSERT INTO periodization (nombre, fecha_inicio, fecha_fin, tipo, notas, color) VALUES (?, ?, ?, ?, ?, ?)"
      )
      ps.setString(1, fixEncoding(nombre))
      ps.setDate(2, Date.valueOf(fechaInicio))
      ps.setDate(3, Date.valueOf(fechaFin))
      ps.setString(4, tipo)
      ps.setString(5, fixEncoding(notas))
      ps.setString(6, periodizationTipoColor(tipo))
      ps.executeUpdate()
    } finally { conn.close() }
  }

  // Sugerencia de periodizacion de los proximos 6 meses respetando la estructura semanal fija
  def generatePeriodizationPlan(): String = {
    val conn = getConnection()
    try {
      val card = getLatestCardData()
      val edad = calcularEdadExacta(card.fechaNacimiento)
      val rsMatches = conn.createStatement().executeQuery(
        "SELECT fecha, rival FROM matches WHERE status='SCHEDULED' ORDER BY fecha ASC LIMIT 20"
      )
      var partidos = List[String]()
      while (rsMatches.next()) partidos = partidos :+ s"${rsMatches.getDate("fecha")}: vs ${fixEncoding(rsMatches.getString("rival"))}"
      val partidosStr = if (partidos.isEmpty) "Sin partidos programados registrados todavia" else partidos.mkString("\n")

      val prompt = s"""Eres preparador físico y planificador deportivo especializado en fútbol base.
Héctor es un portero de $edad años con esta estructura semanal FIJA e inamovible:
- Lunes y Miércoles: Judo
- Martes y Jueves: Entrenamiento colectivo con el equipo (no específico de portero)
- Domingo: Academia específica de porteros
- Sábado: Partido oficial cuando toca

Calendario de partidos ya programados:
$partidosStr

Sugiere una periodización para los próximos 6 meses (bloques de carga alta, descarga, ventanas de evaluación, semanas clave de torneo) que respete estrictamente esta estructura semanal — NO añadas sesiones extra, la carga ya es alta para su edad. Responde en texto plano, en formato de lista breve por mes."""

      AIProvider.ask(prompt)
    } finally { conn.close() }
  }

  // ── MODULO 8: AUDIO-DIARIO (PARTIDO / ACADEMIA) ─────────────────────────
  def getAcademiaSessions(): List[TrainingSession] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("SELECT * FROM trainings WHERE tipo = 'Academia' ORDER BY fecha DESC")
      var list = List[TrainingSession]()
      while (rs.next()) {
        list = list :+ TrainingSession(
          rs.getInt("id"), rs.getDate("fecha").toString, rs.getString("tipo"),
          Option(rs.getString("foco")).getOrElse(""), rs.getInt("rpe"), rs.getInt("calidad"), rs.getInt("atencion"),
          Option(rs.getString("analisis_voz_academia")).getOrElse("")
        )
      }
      list
    } finally { conn.close() }
  }

  def getTrainingById(id: Int): Option[TrainingSession] = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("SELECT * FROM trainings WHERE id = ?")
      ps.setInt(1, id)
      val rs = ps.executeQuery()
      if (rs.next()) Some(TrainingSession(
        rs.getInt("id"), rs.getDate("fecha").toString, rs.getString("tipo"),
        Option(rs.getString("foco")).getOrElse(""), rs.getInt("rpe"), rs.getInt("calidad"), rs.getInt("atencion"),
        Option(rs.getString("analisis_voz_academia")).getOrElse("")
      )) else None
    } finally { conn.close() }
  }

  private def audioDiaryMimeType(audioBase64: String): String =
    if (audioBase64.startsWith("data:audio/webm")) "audio/webm"
    else if (audioBase64.startsWith("data:audio/mp4")) "audio/mp4"
    else if (audioBase64.startsWith("data:audio/mpeg")) "audio/mpeg"
    else if (audioBase64.startsWith("data:audio/ogg")) "audio/ogg"
    else "audio/webm"

  private def audioDiaryDataOnly(audioBase64: String): String =
    if (audioBase64.contains(",")) audioBase64.split(",", 2)(1) else audioBase64

  // Prompt para Hector traduciendo su lenguaje infantil a lenguaje tecnico.
  // BLOQUE G5: edad siempre dinamica via calcularEdadExacta — nunca hardcodeada.
  private def audioDiaryPrompt(contexto: String): String = {
    val edad = calcularEdadExacta(getLatestCardData().fechaNacimiento)
    s"""Eres el psicólogo deportivo y analista técnico de Héctor, un portero de $edad años. El siguiente audio es una grabación espontánea de Héctor hablando libremente después de $contexto. Héctor habla como un niño de $edad años — traduce su lenguaje infantil al lenguaje técnico de un entrenador de porteros de élite. El padre ha grabado sin hacer preguntas guiadas.

Extrae y devuelve en texto plano estas 4 secciones:

ESTADO EMOCIONAL: Detecta su nivel de motivación, confianza o frustración. ¿Está disfrutando? ¿Hay alguna señal de miedo, presión o inseguridad?

PERCEPCIÓN TÉCNICA: ¿Qué aspectos técnicos menciona aunque sea con palabras de niño? ¿Habla de paradas, caídas, salidas, posición? ¿Los describe con seguridad o con duda?

SEÑAL SOCIAL: ¿Menciona a compañeros, al entrenador, a rivales? ¿Hay algo que indique cómo se relaciona con el entorno del equipo o la academia?

CONSEJO PARA EL PADRE: Una acción concreta que el padre puede hacer en las próximas 24 horas para reforzar lo positivo o trabajar lo negativo detectado. Adaptada a $edad años — sin presión, enfocada en el disfrute y la confianza.

Si el audio no contiene información sobre alguna sección escribe 'No mencionado'. Nunca inventes información que no esté en el audio."""
  }

  // Procesamiento efimero: el base64 vive solo en memoria durante esta llamada (nunca a disco).
  def analyzeAudioDiaryMatch(matchId: Int, audioBase64: String): String = {
    val m = getMatchById(matchId)
    val contexto = m.map(mm => s"un partido vs ${fixEncoding(mm.rival)} con resultado ${mm.resultado}")
      .getOrElse("un partido")
    val mime = audioDiaryMimeType(audioBase64)
    val data = audioDiaryDataOnly(audioBase64)
    val res = AIProvider.ask(audioDiaryPrompt(contexto), Some((mime, data)), bypassCache = true)
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("UPDATE matches SET analisis_voz = ? WHERE id = ?")
      ps.setString(1, fixEncoding(res)); ps.setInt(2, matchId); ps.executeUpdate()
    } finally { conn.close() }
    res
  }

  def analyzeAudioDiaryAcademia(trainingId: Int, audioBase64: String): String = {
    val mime = audioDiaryMimeType(audioBase64)
    val data = audioDiaryDataOnly(audioBase64)
    val res = AIProvider.ask(audioDiaryPrompt("una sesión de academia de porteros"), Some((mime, data)), bypassCache = true)
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("UPDATE trainings SET analisis_voz_academia = ? WHERE id = ?")
      ps.setString(1, fixEncoding(res)); ps.setInt(2, trainingId); ps.executeUpdate()
    } finally { conn.close() }
    res
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE A — ANALISIS DE VIDEO REAL CON GEMINI VISION
  // ─────────────────────────────────────────────────────────────────────────────
  // Llamada real a Gemini — SOLO se invoca desde un hilo de fondo tras la subida explicita del usuario
  def analyzeVideoReal(matchId: Int, videoBase64: String, mimeType: String): String = {
    val card = getLatestCardData()
    val edad = calcularEdadExacta(card.fechaNacimiento)

    val prompt = s"""Eres el analista técnico de porteros más experto del mundo, especializado en fútbol base. Analiza el vídeo de Héctor, portero de $edad años en Fútbol 7 prebenjamín. Analiza ÚNICAMENTE las acciones de Héctor como portero. Para cada intervención detectada evalúa: posición de manos (¿palmas hacia el balón, pulgares juntos?), posición de pies (¿rodillas flexionadas, posición de salida?), salidas al balón (¿sale con decisión o duda?), posicionamiento en portería, juego con los pies y comunicación con la defensa. Devuelve en texto plano con estas secciones exactas: ACCIONES DETECTADAS (lista de intervenciones vistas), PUNTOS FUERTES (máximo 3 aspectos técnicos que hace bien con timestamp si puedes), PUNTOS A MEJORAR (máximo 3 errores técnicos con descripción concreta de qué hace mal y cómo debería hacerlo), EJERCICIO RECOMENDADO (un ejercicio concreto para el próximo entrenamiento de academia basado en el error más frecuente), NOTA TÉCNICA GLOBAL (nota del 1 al 10 con justificación en una frase). Si no puedes identificar claramente al portero o la calidad no permite análisis preciso, indícalo. Al final añade UNA última línea exactamente con este formato, puntuando de 1 (muy débil) a 5 (excelente) las mismas cinco dimensiones que usa la rúbrica del padre: RUBRICA_IA: posicion=N; decisiones=N; pies=N; comunicacion=N; actitud=N"""

    val res = AIProvider.ask(prompt, Some((mimeType, videoBase64)), bypassCache = true)
    val conn = getConnection()
    val rivalPartido = try {
      val ps = conn.prepareStatement("UPDATE matches SET video_analisis_ia = ?, video_analisis_fecha = NOW() WHERE id = ?")
      ps.setString(1, fixEncoding(res)); ps.setInt(2, matchId)
      ps.executeUpdate()
      val rsR = conn.prepareStatement("SELECT rival FROM matches WHERE id = ?")
      rsR.setInt(1, matchId)
      val rr = rsR.executeQuery()
      if (rr.next()) fixEncoding(rr.getString("rival")) else ""
    } finally { conn.close() }
    // BLOQUE G3: notificacion Telegram — ya estamos en el hilo de fondo del analisis de video
    val notaTecnica = extractNotaTecnica(res).map(n => f"$n%.1f").getOrElse("—")
    TelegramService.enviar(s"🎬 Análisis de vídeo listo — vs $rivalPartido. Nota técnica: $notaTecnica/10")
    res
  }

  // Lectura desde BD unicamente — nunca llama a Gemini
  def getVideoAnalysisStatus(matchId: Int): Map[String, Any] = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("SELECT video_analisis_ia, video_analisis_fecha FROM matches WHERE id = ?")
      ps.setInt(1, matchId)
      val rs = ps.executeQuery()
      if (rs.next()) {
        val analisis = Option(rs.getString("video_analisis_ia")).filter(_.nonEmpty)
        analisis match {
          case Some(a) => Map("status" -> "done", "analisis" -> a,
            "fecha" -> Option(rs.getTimestamp("video_analisis_fecha")).map(_.toString).getOrElse(""))
          case None => Map("status" -> "pending")
        }
      } else Map("status" -> "pending")
    } finally { conn.close() }
  }

  private val videoAnalysisSecciones = Seq("ACCIONES DETECTADAS", "PUNTOS FUERTES", "PUNTOS A MEJORAR", "EJERCICIO RECOMENDADO", "NOTA TÉCNICA GLOBAL")

  def parseVideoAnalysisSections(texto: String): Map[String, String] = {
    val upper = texto.toUpperCase
    videoAnalysisSecciones.zipWithIndex.map { case (sec, idx) =>
      val startIdx = upper.indexOf(sec)
      if (startIdx < 0) sec -> ""
      else {
        val contentStart = startIdx + sec.length
        val nextIdx = videoAnalysisSecciones.drop(idx + 1)
          .flatMap(s => { val i = upper.indexOf(s, contentStart); if (i >= 0) Some(i) else None })
          .headOption.getOrElse(texto.length)
        // la linea RUBRICA_IA es para el cruce con la rubrica del padre, no para mostrarla
        sec -> texto.substring(contentStart, nextIdx).linesIterator.filterNot(_.toUpperCase.contains("RUBRICA_IA")).mkString("\n").trim.stripPrefix(":").trim
      }
    }.toMap
  }

  // BLOQUE B3: puntuacion 1-5 de la IA en las dimensiones de la rubrica (linea RUBRICA_IA del analisis)
  val dimensionesRubrica: Seq[(String, String, String)] = Seq( // clave IA, columna, etiqueta
    ("posicion", "rubrica_posicion", "Posición"), ("decisiones", "rubrica_decisiones", "Decisiones bajo presión"),
    ("pies", "rubrica_pies", "Juego con los pies"), ("comunicacion", "rubrica_comunicacion", "Comunicación"),
    ("actitud", "rubrica_actitud", "Actitud y concentración"))

  def extractRubricaIA(texto: String): Option[Map[String, Int]] =
    texto.linesIterator.find(_.toUpperCase.contains("RUBRICA_IA")).flatMap { linea =>
      val valores = """(?i)(posicion|decisiones|pies|comunicacion|actitud)\s*=\s*([1-5])""".r
        .findAllMatchIn(linea).map(m => m.group(1).toLowerCase -> m.group(2).toInt).toMap
      if (valores.size == 5) Some(valores) else None
    }

  def extractNotaTecnica(texto: String): Option[Double] = {
    val seccion = parseVideoAnalysisSections(texto).getOrElse("NOTA TÉCNICA GLOBAL", "")
    """(\d+(?:[.,]\d+)?)""".r.findFirstIn(seccion).map(_.replace(",", ".").toDouble)
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE D — ANALISIS DE VIDEO REAL EN ENTRENAMIENTOS CON GEMINI VISION
  // ─────────────────────────────────────────────────────────────────────────────
  // Llamada real a Gemini — SOLO se invoca desde un hilo de fondo tras la subida explicita del usuario
  def analyzeVideoTraining(trainingId: Int, videoBase64: String, mimeType: String): String = {
    val card = getLatestCardData()
    val edad = calcularEdadExacta(card.fechaNacimiento)

    val prompt = s"""Eres el analista técnico de porteros más experto del mundo, especializado en fútbol base. Analiza este vídeo de entrenamiento de Héctor, portero de $edad años. El vídeo está enfocado exclusivamente en Héctor. A diferencia de un partido, aquí puedes ver repeticiones del mismo gesto — analiza la progresión técnica a lo largo de la sesión. Evalúa: qué ejercicio o gesto técnico se está trabajando, si la ejecución mejora entre la primera y la última repetición, el error técnico más frecuente en todas las repeticiones con descripción de por qué es un error y cómo corregirlo, y algo que hace consistentemente bien. Devuelve en texto plano: EJERCICIO DETECTADO: [qué se estaba trabajando] / PROGRESIÓN: [¿mejora a lo largo de la sesión? describe con detalle] / ERROR RECURRENTE: [el error más frecuente con explicación técnica y corrección concreta] / PUNTO FUERTE: [algo que hace bien de forma consistente] / RECOMENDACIÓN ACADEMIA: [una instrucción concreta para el entrenador de academia basada en lo visto]. Si la calidad del vídeo no permite análisis preciso, indícalo."""

    val res = AIProvider.ask(prompt, Some((mimeType, videoBase64)), bypassCache = true)
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("UPDATE trainings SET video_analisis_ia = ?, video_analisis_fecha = NOW() WHERE id = ?")
      ps.setString(1, fixEncoding(res)); ps.setInt(2, trainingId)
      ps.executeUpdate()
    } finally { conn.close() }
    res
  }

  // Lectura desde BD unicamente — nunca llama a Gemini
  def getVideoAnalysisStatusTraining(trainingId: Int): Map[String, Any] = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("SELECT video_analisis_ia, video_analisis_fecha FROM trainings WHERE id = ?")
      ps.setInt(1, trainingId)
      val rs = ps.executeQuery()
      if (rs.next()) {
        val analisis = Option(rs.getString("video_analisis_ia")).filter(_.nonEmpty)
        analisis match {
          case Some(a) => Map("status" -> "done", "analisis" -> a,
            "fecha" -> Option(rs.getTimestamp("video_analisis_fecha")).map(_.toString).getOrElse(""))
          case None => Map("status" -> "pending")
        }
      } else Map("status" -> "pending")
    } finally { conn.close() }
  }

  private val videoAnalysisSeccionesTraining = Seq("EJERCICIO DETECTADO", "PROGRESIÓN", "ERROR RECURRENTE", "PUNTO FUERTE", "RECOMENDACIÓN ACADEMIA")

  def parseVideoAnalysisSectionsTraining(texto: String): Map[String, String] = {
    val upper = texto.toUpperCase
    videoAnalysisSeccionesTraining.zipWithIndex.map { case (sec, idx) =>
      val startIdx = upper.indexOf(sec)
      if (startIdx < 0) sec -> ""
      else {
        val contentStart = startIdx + sec.length
        val nextIdx = videoAnalysisSeccionesTraining.drop(idx + 1)
          .flatMap(s => { val i = upper.indexOf(s, contentStart); if (i >= 0) Some(i) else None })
          .headOption.getOrElse(texto.length)
        sec -> texto.substring(contentStart, nextIdx).trim.stripPrefix(":").trim
      }
    }.toMap
  }

  // ── BLOQUE A4: HISTORIAL DE ANALISIS DE VIDEO ───────────────────────────
  def getVideoAnalysisHistory(): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("""
        SELECT id, rival, fecha, video_analisis_ia, video_analisis_fecha
        FROM matches WHERE status = 'PLAYED' AND video_analisis_ia IS NOT NULL AND video_analisis_ia <> ''
        ORDER BY fecha ASC
      """)
      var list = List[Map[String, Any]]()
      while (rs.next()) {
        val analisis = rs.getString("video_analisis_ia")
        list = list :+ Map[String, Any](
          "matchId" -> rs.getInt("id"),
          "rival" -> fixEncoding(rs.getString("rival")),
          "fecha" -> rs.getDate("fecha").toString,
          "notaTecnica" -> extractNotaTecnica(analisis),
          "analisis" -> analisis
        )
      }
      list
    } finally { conn.close() }
  }

  // ── BLOQUE D: HISTORIAL COMBINADO DE VIDEO (PARTIDOS + ENTRENAMIENTOS) ──
  // Los entrenamientos no tienen "nota tecnica" en su analisis (hay repeticiones, no una
  // valoracion 1-10) asi que se usa la "calidad" registrada en la sesion como proxy comparable.
  def getVideoAnalysisHistoryAll(): List[Map[String, Any]] = {
    val matches = getVideoAnalysisHistory().map { h =>
      Map[String, Any](
        "tipoVideo"   -> "Partido",
        "itemId"      -> h("matchId"),
        "label"       -> h("rival"),
        "fecha"       -> h("fecha"),
        "notaTecnica" -> h("notaTecnica"),
        "analisis"    -> h("analisis")
      )
    }
    val conn = getConnection()
    val trainingsList = try {
      val rs = conn.createStatement().executeQuery("""
        SELECT id, fecha, tipo, foco, calidad, video_analisis_ia
        FROM trainings WHERE video_analisis_ia IS NOT NULL AND video_analisis_ia <> ''
        ORDER BY fecha ASC
      """)
      var list = List[Map[String, Any]]()
      while (rs.next()) {
        val foco = Option(rs.getString("foco")).filter(_.nonEmpty).map(f => " · " + fixEncoding(f)).getOrElse("")
        list = list :+ Map[String, Any](
          "tipoVideo"   -> "Entreno",
          "itemId"      -> rs.getInt("id"),
          "label"       -> (fixEncoding(Option(rs.getString("tipo")).getOrElse("Entreno")) + foco),
          "fecha"       -> rs.getDate("fecha").toString,
          "notaTecnica" -> Some(rs.getInt("calidad").toDouble),
          "analisis"    -> rs.getString("video_analisis_ia")
        )
      }
      list
    } finally { conn.close() }
    (matches ++ trainingsList).sortBy(_("fecha").asInstanceOf[String])
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE E — VIDEO IA ALIMENTA FLASH-CARDS Y ACADEMIA (solo Elite, SQL/cache, sin Gemini)
  // ─────────────────────────────────────────────────────────────────────────────
  def getUltimoErrorRecurrente(): Option[String] = {
    val hist = getVideoAnalysisHistoryAll().sortBy(_("fecha").asInstanceOf[String]).takeRight(3)
    if (hist.isEmpty) return None

    def extraerError(h: Map[String, Any]): String = {
      val analisis = h("analisis").asInstanceOf[String]
      val texto =
        if (h("tipoVideo").asInstanceOf[String] == "Partido") parseVideoAnalysisSections(analisis).getOrElse("PUNTOS A MEJORAR", "")
        else parseVideoAnalysisSectionsTraining(analisis).getOrElse("ERROR RECURRENTE", "")
      texto.trim
    }

    val errores = hist.map(extraerError).filter(_.nonEmpty)
    if (errores.isEmpty) return None
    val masReciente = errores.last
    if (errores.size < 2) return Some(masReciente)

    // Heurística ligera: si el error mas reciente comparte una palabra clave tecnica con algun
    // analisis anterior, se marca como patron repetido (los textos son parrafos libres de Gemini,
    // no hay forma exacta de comparar sin otra llamada a IA — se evita por la regla "nunca en render").
    val keywords = Seq("mano", "pie", "salida", "posicion", "comunicac", "blocaje", "reflejo", "colocacion", "anticipac", "aereo", "concentrac")
    val keyReciente = keywords.find(k => masReciente.toLowerCase.contains(k))
    val repiteEnAnterior = keyReciente.exists(k => errores.dropRight(1).exists(_.toLowerCase.contains(k)))
    if (repiteEnAnterior) Some(s"$masReciente (patrón repetido en los últimos análisis)") else Some(masReciente)
  }

  // Lectura desde cache unicamente — nunca llama a Gemini en el render de pagina
  def getVideoEvolutionAnalysisCached(): Option[String] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT payload FROM feature_cache WHERE cache_key = 'video_evolution_ia' AND updated_at > NOW() - INTERVAL '14 days'"
      )
      if (rs.next()) Some(ujson.read(rs.getString("payload"))("analisis").str) else None
    } finally { conn.close() }
  }

  // Llamada real a Gemini — SOLO se invoca desde el boton POST "Evolución técnica IA"
  def generateVideoEvolutionAnalysis(): String = {
    val conn = getConnection()
    try {
      val hist = getVideoAnalysisHistory()
      if (hist.size < 2) return "Se necesitan al menos 2 análisis de vídeo para evaluar evolución."

      val card = getLatestCardData()
      val edad = calcularEdadExacta(card.fechaNacimiento)
      val fechaInicio = hist.head("fecha").asInstanceOf[String]
      val fechaFin = hist.last("fecha").asInstanceOf[String]
      val n = hist.size

      val analisisConcatenados = hist.map { h =>
        val fecha = h("fecha").asInstanceOf[String]
        val secciones = parseVideoAnalysisSections(h("analisis").asInstanceOf[String])
        s"[$fecha] Fuertes: ${secciones.getOrElse("PUNTOS FUERTES", "")} | A mejorar: ${secciones.getOrElse("PUNTOS A MEJORAR", "")}"
      }.mkString("\n")

      val prompt = s"""Tienes $n análisis de vídeo de Héctor portero de $edad años desde $fechaInicio hasta $fechaFin. Detecta: 1) Si los errores técnicos del principio se han corregido, 2) Qué aspecto técnico ha mejorado más, 3) Qué aspecto lleva más tiempo sin mejorar y necesita atención prioritaria. Máximo 3 líneas por punto.

Datos:
$analisisConcatenados"""

      val analisis = AIProvider.ask(prompt, None, bypassCache = true)
      val payload = ujson.Obj("analisis" -> analisis)
      val upsert = conn.prepareStatement("""
        INSERT INTO feature_cache (cache_key, payload, updated_at) VALUES ('video_evolution_ia', ?, NOW())
        ON CONFLICT (cache_key) DO UPDATE SET payload = EXCLUDED.payload, updated_at = NOW()
      """)
      upsert.setString(1, ujson.write(payload))
      upsert.executeUpdate()
      analisis
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE B — IDP: PLAN DE DESARROLLO INDIVIDUAL
  // ─────────────────────────────────────────────────────────────────────────────

  // Llamada real a Gemini — SOLO se invoca desde el boton POST "Generar IDP" (hilo de fondo)
  def generarIDP(temporada: String, fechaInicio: String, fechaFin: String): Int = {
    val conn = getConnection()
    try {
      val card = getLatestCardData()
      val edad = calcularEdadExacta(card.fechaNacimiento)

      val skills = getGoalkeeperSkills()
      val pctChecklist = if (skills.nonEmpty) skills.count(_.conseguido) * 100 / skills.size else 0
      val checklistPorCategoria = skills.groupBy(_.categoria).map { case (cat, l) =>
        val pct = if (l.nonEmpty) l.count(_.conseguido) * 100 / l.size else 0
        s"$cat=$pct%"
      }.mkString(", ")

      val faseBio = try getBioBandingData().getOrElse("faseBio", "Sin datos").toString catch { case _: Exception => "Sin datos" }

      val tests = getPhysicalTests()
      val ultimoTest = tests.lastOption.map { t =>
        val v10 = t("velocidad10m").asInstanceOf[Option[Double]].map(v => f"$v%.2fs").getOrElse("—")
        val sv  = t("saltoVertical").asInstanceOf[Option[Int]].map(v => s"${v}cm").getOrElse("—")
        s"10m=$v10, salto=$sv (${t("fecha")})"
      }.getOrElse("Sin tests físicos registrados")

      val learning = getLearningVelocityIndex()
      val indiceAprendizaje = if (learning.getOrElse("suficiente", false).asInstanceOf[Boolean])
        f"${learning("indice").asInstanceOf[Double]}%.0f (media ${learning("mediaDias").asInstanceOf[Double]}%.0f dias/habilidad)"
      else "Sin datos suficientes"

      val psych = getPsychRecords()
      val ultimoPsych = psych.lastOption.map { p =>
        s"motivacion=${p("motivacion")}/5, disfrute=${p("disfrute")}/5, miedo fracaso=${p("miedoFracaso")}/5 (${p("fecha")})"
      }.getOrElse("Sin registros psicologicos")

      val anioActual = LocalDate.now().getYear.toString
      val oportunidadesAnio = getOpportunities().count(_.fecha.take(4) == anioActual)

      val acute = getWorkloads(7)
      val chronic = getWorkloads(28)
      val acwr = StatsCalculator.calculateACWR(acute, chronic)

      // MODULO ARQUETIPO: contexto para que el objetivo tecnico del IDP sea coherente con el perfil natural
      val arquetipoLine = {
        val arq = calcularArquetipoPortero()
        if (arq("activo").asInstanceOf[Boolean]) {
          val nombre = arquetipoDescripcion(arq("dominante").asInstanceOf[String])("nombre")
          s" Su arquetipo dominante es $nombre. El objetivo técnico del IDP debe ser coherente con este perfil."
        } else ""
      }

      val prompt = s"""Eres el director de desarrollo de jugadores de una academia de élite. Héctor es un portero de $edad años con estos datos actuales: checklist técnico conseguido $pctChecklist% (por categoría: $checklistPorCategoria), fase madurativa $faseBio, último test físico: $ultimoTest, índice de velocidad de aprendizaje: $indiceAprendizaje, último registro psicológico: $ultimoPsych, oportunidades de visibilidad este año: $oportunidadesAnio, ACWR medio últimas 4 semanas: ${f"$acwr%.2f"}.$arquetipoLine Genera exactamente 4 objetivos SMART para la temporada $temporada, uno por cada dimensión. Para cada objetivo devuelve en formato: DIMENSION|OBJETIVO|METRICA|VALOR_ACTUAL|VALOR_OBJETIVO|FECHA_LIMITE. Ejemplos: TECNICO|Consolidar el 80% del checklist de técnica básica|% habilidades técnicas conseguidas|45%|80%|2027-01-31. FISICO|Mantener ACWR en zona verde toda la temporada|% semanas con ACWR menor de 1.3|Sin datos|85%|2027-06-30. MENTAL|Registrar motivación igual o mayor a 4 en todos los registros trimestrales|Puntuación motivación|Sin datos|4/5|2027-06-30. VISIBILIDAD|Participar en 2 eventos de visibilidad ALTO|Eventos nivel ALTO participados|0|2|2027-05-31. Sé específico y realista para la edad de Héctor. Devuelve SOLO las 4 líneas en el formato indicado, sin texto adicional."""

      val respuesta = AIProvider.ask(prompt, None, bypassCache = true)

      val insTemp = conn.prepareStatement(
        "INSERT INTO idp_temporadas (temporada, fecha_inicio, fecha_fin, estado) VALUES (?, ?::date, ?::date, 'ACTIVA') RETURNING id"
      )
      insTemp.setString(1, fixEncoding(temporada)); insTemp.setString(2, fechaInicio); insTemp.setString(3, fechaFin)
      val rsId = insTemp.executeQuery()
      rsId.next()
      val temporadaId = rsId.getInt(1)

      val insObj = conn.prepareStatement("""
        INSERT INTO idp_objetivos (temporada_id, dimension, objetivo, metrica, valor_actual, valor_objetivo, fecha_limite)
        VALUES (?, ?, ?, ?, ?, ?, ?::date)
      """)
      respuesta.split("\n").map(_.trim).filter(_.nonEmpty).foreach { linea =>
        val partes = linea.split("\\|").map(_.trim)
        if (partes.length >= 6) {
          insObj.setInt(1, temporadaId)
          insObj.setString(2, partes(0).toUpperCase)
          insObj.setString(3, fixEncoding(partes(1)))
          insObj.setString(4, fixEncoding(partes(2)))
          insObj.setString(5, fixEncoding(partes(3)))
          insObj.setString(6, fixEncoding(partes(4)))
          val fechaLim = try { LocalDate.parse(partes(5)); partes(5) } catch { case _: Exception => fechaFin }
          insObj.setString(7, fechaLim)
          insObj.executeUpdate()
        }
      }
      temporadaId
    } finally { conn.close() }
  }

  // Calculo de progreso — SQL/matematicas puras, sin Gemini. Seguro de llamar en cada carga de pagina.
  def actualizarProgresoIDP(temporadaId: Int): Unit = {
    val conn = getConnection()
    try {
      val rsTemp = conn.prepareStatement("SELECT fecha_inicio FROM idp_temporadas WHERE id = ?")
      rsTemp.setInt(1, temporadaId)
      val rs0 = rsTemp.executeQuery()
      if (!rs0.next()) return
      val fechaInicio = rs0.getDate("fecha_inicio").toLocalDate

      val rsObj = conn.prepareStatement("SELECT id, dimension, notas FROM idp_objetivos WHERE temporada_id = ?")
      rsObj.setInt(1, temporadaId)
      val rsO = rsObj.executeQuery()
      var objetivos = List[(Int, String, String)]()
      while (rsO.next()) objetivos = objetivos :+ (rsO.getInt("id"), rsO.getString("dimension").toUpperCase, Option(rsO.getString("notas")).getOrElse(""))

      // Fuentes de datos calculadas una sola vez
      val skills = getGoalkeeperSkills()
      val pctChecklist = if (skills.nonEmpty) skills.count(_.conseguido) * 100 / skills.size else 0

      val acwrSemanasPct: Int = {
        var semanasOk = 0; var semanasTotal = 0
        var cursor = fechaInicio
        val hoy = LocalDate.now()
        while (!cursor.isAfter(hoy)) {
          val finSemana = cursor.plusDays(6)
          val acuteEnd = if (finSemana.isAfter(hoy)) hoy else finSemana
          val diasAcute = math.max(1, java.time.temporal.ChronoUnit.DAYS.between(cursor, acuteEnd).toInt + 1)
          val rsAcute = conn.createStatement().executeQuery(
            s"SELECT COALESCE(SUM(minutos),0) as m FROM matches WHERE status='PLAYED' AND fecha >= '${cursor}' AND fecha <= '${acuteEnd}'")
          val acuteLoad = if (rsAcute.next()) rsAcute.getDouble("m") else 0.0
          val chronicStart = cursor.minusDays(21)
          val rsChronic = conn.createStatement().executeQuery(
            s"SELECT COALESCE(SUM(minutos),0) as m FROM matches WHERE status='PLAYED' AND fecha >= '${chronicStart}' AND fecha <= '${acuteEnd}'")
          val chronicLoad = if (rsChronic.next()) rsChronic.getDouble("m") / 4.0 else 0.0
          val acwrSemana = if (chronicLoad > 0) acuteLoad / chronicLoad else 1.0
          semanasTotal += 1
          if (acwrSemana < 1.3) semanasOk += 1
          cursor = cursor.plusWeeks(1)
        }
        if (semanasTotal > 0) semanasOk * 100 / semanasTotal else 0
      }

      val rsMotiv = conn.createStatement().executeQuery(
        s"SELECT AVG(motivacion) as m FROM psych_records WHERE fecha >= '${fechaInicio}'")
      val motivacionProm = if (rsMotiv.next()) rsMotiv.getDouble("m") else 0.0

      val rsVis = conn.createStatement().executeQuery(
        s"SELECT COUNT(*) as c FROM visibility_events WHERE nivel_visibilidad = 'ALTO' AND participamos = TRUE AND fecha >= '${fechaInicio}'")
      val eventosAlto = if (rsVis.next()) rsVis.getInt("c") else 0

      def parseObjetivoNum(s: String): Double =
        """(\d+(?:[.,]\d+)?)""".r.findFirstIn(s).map(_.replace(",", ".").toDouble).getOrElse(0.0)

      objetivos.foreach { case (objId, dimension, notasActuales) =>
        val (valorActualOpt, progresoOpt): (Option[String], Option[Int]) = dimension match {
          case "TECNICO" =>
            (Some(s"$pctChecklist%"), Some(pctChecklist))
          case "FISICO" =>
            (Some(s"$acwrSemanasPct%"), Some(acwrSemanasPct))
          case "MENTAL" =>
            val valorActual = f"${motivacionProm}%.1f/5"
            val progreso = if (motivacionProm > 0) math.min(100, (motivacionProm / 5.0 * 100).toInt) else 0
            (Some(valorActual), Some(progreso))
          case "VISIBILIDAD" =>
            val rsObjetivoVal = conn.prepareStatement("SELECT valor_objetivo FROM idp_objetivos WHERE id = ?")
            rsObjetivoVal.setInt(1, objId)
            val rsOV = rsObjetivoVal.executeQuery()
            val objetivoNum = if (rsOV.next()) parseObjetivoNum(rsOV.getString("valor_objetivo")) else 0.0
            val progreso = if (objetivoNum > 0) math.min(100, (eventosAlto * 100 / objetivoNum).toInt) else 0
            (Some(eventosAlto.toString), Some(progreso))
          case _ => (None, None)
        }

        (valorActualOpt, progresoOpt) match {
          case (Some(valorActual), Some(progreso)) =>
            val nuevoEstado = if (progreso >= 100) "CONSEGUIDO" else "EN_CURSO"
            val rsCheckEstado = conn.prepareStatement("SELECT estado FROM idp_objetivos WHERE id = ?")
            rsCheckEstado.setInt(1, objId)
            val rsCE = rsCheckEstado.executeQuery()
            val estadoActual = if (rsCE.next()) rsCE.getString("estado") else "EN_CURSO"
            val estadoFinal = if (estadoActual == "AJUSTADO") estadoActual else nuevoEstado

            val upd = conn.prepareStatement("UPDATE idp_objetivos SET valor_actual = ?, progreso_pct = ?, estado = ? WHERE id = ?")
            upd.setString(1, valorActual); upd.setInt(2, progreso); upd.setString(3, estadoFinal); upd.setInt(4, objId)
            upd.executeUpdate()
          case _ =>
            if (!notasActuales.contains("Actualizar manualmente")) {
              val notaFinal = if (notasActuales.trim.isEmpty) "Actualizar manualmente" else notasActuales
              val upd = conn.prepareStatement("UPDATE idp_objetivos SET notas = ? WHERE id = ?")
              upd.setString(1, notaFinal); upd.setInt(2, objId)
              upd.executeUpdate()
            }
        }
      }
    } finally { conn.close() }
  }

  def getActiveIdpTemporada(): Option[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT * FROM idp_temporadas WHERE estado = 'ACTIVA' ORDER BY id DESC LIMIT 1")
      if (rs.next()) Some(Map(
        "id" -> rs.getInt("id"),
        "temporada" -> fixEncoding(rs.getString("temporada")),
        "fechaInicio" -> rs.getDate("fecha_inicio").toString,
        "fechaFin" -> rs.getDate("fecha_fin").toString,
        "estado" -> rs.getString("estado"),
        "resumenIa" -> Option(rs.getString("resumen_ia")).getOrElse("")
      )) else None
    } finally { conn.close() }
  }

  def getIdpObjetivos(temporadaId: Int): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("SELECT * FROM idp_objetivos WHERE temporada_id = ? ORDER BY dimension ASC")
      ps.setInt(1, temporadaId)
      val rs = ps.executeQuery()
      var list = List[Map[String, Any]]()
      while (rs.next()) {
        list = list :+ Map[String, Any](
          "id" -> rs.getInt("id"),
          "dimension" -> rs.getString("dimension"),
          "objetivo" -> fixEncoding(rs.getString("objetivo")),
          "metrica" -> fixEncoding(rs.getString("metrica")),
          "valorActual" -> fixEncoding(Option(rs.getString("valor_actual")).getOrElse("")),
          "valorObjetivo" -> fixEncoding(rs.getString("valor_objetivo")),
          "fechaLimite" -> rs.getDate("fecha_limite").toString,
          "progresoPct" -> rs.getInt("progreso_pct"),
          "estado" -> rs.getString("estado"),
          "notas" -> fixEncoding(Option(rs.getString("notas")).getOrElse(""))
        )
      }
      list
    } finally { conn.close() }
  }

  def getIdpRevisiones(temporadaId: Int): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("SELECT * FROM idp_revisiones WHERE temporada_id = ? ORDER BY fecha DESC, id DESC")
      ps.setInt(1, temporadaId)
      val rs = ps.executeQuery()
      var list = List[Map[String, Any]]()
      while (rs.next()) {
        list = list :+ Map[String, Any](
          "id" -> rs.getInt("id"),
          "fecha" -> rs.getDate("fecha").toString,
          "tipo" -> rs.getString("tipo"),
          "resumen" -> fixEncoding(Option(rs.getString("resumen")).getOrElse("")),
          "ajustes" -> fixEncoding(Option(rs.getString("ajustes")).getOrElse("")),
          "analisisIa" -> fixEncoding(Option(rs.getString("analisis_ia")).getOrElse(""))
        )
      }
      list
    } finally { conn.close() }
  }

  def updateIdpObjetivoNotas(id: Int, notas: String): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("UPDATE idp_objetivos SET notas = ? WHERE id = ?")
      ps.setString(1, fixEncoding(notas)); ps.setInt(2, id)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def ajustarIdpObjetivo(id: Int, nuevoValorObjetivo: String, nuevaFechaLimite: String): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("UPDATE idp_objetivos SET valor_objetivo = ?, fecha_limite = ?::date, estado = 'AJUSTADO' WHERE id = ?")
      ps.setString(1, fixEncoding(nuevoValorObjetivo)); ps.setString(2, nuevaFechaLimite); ps.setInt(3, id)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  // Guarda la revision (sin Gemini) y devuelve su id — el analisis IA se lanza aparte en background
  def saveIdpRevision(temporadaId: Int, tipo: String, resumen: String, ajustes: String): Int = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("""
        INSERT INTO idp_revisiones (temporada_id, tipo, resumen, ajustes) VALUES (?, ?, ?, ?) RETURNING id
      """)
      ps.setInt(1, temporadaId); ps.setString(2, tipo); ps.setString(3, fixEncoding(resumen)); ps.setString(4, fixEncoding(ajustes))
      val rs = ps.executeQuery()
      if (rs.next()) rs.getInt(1) else -1
    } finally { conn.close() }
  }

  // Llamada real a Gemini — SOLO se invoca en background tras guardar una revision
  def generateIdpRevisionAnalysis(revisionId: Int): String = {
    val conn = getConnection()
    try {
      val rsRev = conn.prepareStatement("SELECT temporada_id, tipo, resumen FROM idp_revisiones WHERE id = ?")
      rsRev.setInt(1, revisionId)
      val rsR = rsRev.executeQuery()
      if (!rsR.next()) return ""
      val temporadaId = rsR.getInt("temporada_id")
      val tipoRevision = rsR.getString("tipo")
      val resumenPadre = fixEncoding(Option(rsR.getString("resumen")).getOrElse(""))

      val card = getLatestCardData()
      val edad = calcularEdadExacta(card.fechaNacimiento)
      val objetivos = getIdpObjetivos(temporadaId)
      val objetivosStr = objetivos.map(o => s"${o("dimension")}: ${o("objetivo")} (progreso ${o("progresoPct")}%)").mkString("; ")
      val progresoStr = objetivos.map(o => s"${o("dimension")}=${o("progresoPct")}%").mkString(", ")

      val prompt = s"""Héctor es un portero de $edad años. Sus objetivos de temporada son: $objetivosStr. En la revisión de $tipoRevision el progreso es: $progresoStr. El padre añade: $resumenPadre. Genera: 1) Qué objetivo va mejor, 2) Cuál necesita más atención, 3) Recomendación concreta para el próximo mes. Máximo 2 líneas por punto."""

      val analisis = AIProvider.ask(prompt, None, bypassCache = true)
      val upd = conn.prepareStatement("UPDATE idp_revisiones SET analisis_ia = ? WHERE id = ?")
      upd.setString(1, fixEncoding(analisis)); upd.setInt(2, revisionId)
      upd.executeUpdate()
      analisis
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE A — DEUDA DE SUENO ACUMULADA SEMANAL
  // ─────────────────────────────────────────────────────────────────────────────
  // Calculo SQL/matematicas puras — sin Gemini, seguro de llamar en el render de pagina.
  def calcularDeudaSueno(): Map[String, Any] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("""
        SELECT fecha::date as dia, horas_sueno, sueno_profundo_min
        FROM wellness
        WHERE fecha::date >= CURRENT_DATE - INTERVAL '7 days'
        ORDER BY fecha DESC
      """)
      var diasConDatos = 0
      var horasTotales = 0.0
      var profundoTotal = 0
      while (rs.next()) {
        val h = rs.getDouble("horas_sueno")
        if (h > 0) { horasTotales += h; diasConDatos += 1 }
        val p = rs.getInt("sueno_profundo_min")
        if (p > 0) profundoTotal += p
      }
      val horasOptimas = 10.0
      val horasEsperadas = horasOptimas * diasConDatos
      val deudaHoras = Math.max(0.0, horasEsperadas - horasTotales)
      val mediaDiaria = if (diasConDatos > 0) horasTotales / diasConDatos else 0.0
      val nivel = if (deudaHoras < 2) "MINIMA"
        else if (deudaHoras < 5) "MODERADA"
        else if (deudaHoras < 8) "ALTA"
        else "CRITICA"
      Map(
        "diasConDatos"  -> diasConDatos,
        "horasTotales"  -> horasTotales,
        "mediaDiaria"   -> mediaDiaria,
        "deudaHoras"    -> deudaHoras,
        "nivel"         -> nivel,
        "profundoMedio" -> (if (diasConDatos > 0) profundoTotal / diasConDatos else 0)
      )
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE A — INDICE DE FORMA DIARIO
  // ─────────────────────────────────────────────────────────────────────────────
  // Calculo SQL/matematicas puras — sin Gemini, seguro de llamar en el render de pagina.
  def calcularFormaHoy(): Map[String, Any] = {
    val conn = getConnection()
    try {
      val rsW = conn.createStatement().executeQuery(
        "SELECT horas_sueno, sueno_profundo_min, energia, animo, fc_reposo, somnolencia FROM wellness WHERE fecha >= CURRENT_DATE - 1 ORDER BY fecha DESC LIMIT 1")
      var somnolenciaHoy: Option[Int] = None
      val (horasSueno, suenoProfundoMin, energiaW, animoW, fcReposoHoy) = if (rsW.next()) {
        somnolenciaHoy = Option(rsW.getObject("somnolencia")).map(_ => rsW.getInt("somnolencia"))
        val spObj = rsW.getObject("sueno_profundo_min")
        val sp = if (spObj == null) None else Some(rsW.getInt("sueno_profundo_min"))
        val fcObj = rsW.getObject("fc_reposo")
        val fc = if (fcObj == null) None else Some(rsW.getInt("fc_reposo"))
        // NULL (fila creada solo con sueno o FC, p.ej. desde Telegram) cuenta como neutro, no como 0
        val energiaOpt = Option(rsW.getObject("energia")).map(_ => rsW.getInt("energia")).getOrElse(3)
        val animoOpt = Option(rsW.getObject("animo")).map(_ => rsW.getInt("animo")).getOrElse(3)
        (rsW.getDouble("horas_sueno"), sp, energiaOpt, animoOpt, fc)
      } else (0.0, None: Option[Int], 3, 3, None: Option[Int])

      // FIX 2: si el historico es insuficiente (<3 semanas con datos), acwr_score neutro en vez de penalizar
      val acwrEstado = calcularACWRConEstado()
      val acwrInsuficiente = acwrEstado("status").asInstanceOf[String] == "INSUFICIENTE"
      val acwr = acwrEstado("acwr").asInstanceOf[Double]

      // BLOQUE B3: el partido de referencia para "dias desde el ultimo partido" debe ser de la temporada activa
      val rsUltimo = conn.createStatement().executeQuery(
        s"SELECT MAX(fecha) as f FROM matches WHERE status='PLAYED' ${seasonFilter(getTemporadaActivaId())}")
      val diasDesdePartido: Option[Int] =
        if (rsUltimo.next()) {
          val f = rsUltimo.getDate("f")
          if (f == null) None else Some(java.time.temporal.ChronoUnit.DAYS.between(f.toLocalDate, LocalDate.now()).toInt)
        } else None

      val faseBio = try getBioBandingData().getOrElse("faseBio", "").toString catch { case _: Exception => "" }

      val suenoScore = formaSuenoScore(suenoProfundoMin, horasSueno)
      val energiaScore = energiaW * 2.0
      val animoScore = animoW * 2.0
      val acwrScore = formaAcwrScore(acwr, acwrInsuficiente)
      val descansoScore = formaDescansoScore(diasDesdePartido)
      val phvScore = formaPhvScore(faseBio)

      // BLOQUE A — FC REPOSO: solo se incorpora al indice si hay >=10 registros historicos
      val rsFcCount = conn.createStatement().executeQuery(
        "SELECT COUNT(*) as c FROM wellness WHERE fc_reposo IS NOT NULL")
      val fcHistCount = if (rsFcCount.next()) rsFcCount.getInt("c") else 0

      val fcScore: Option[Double] = if (fcHistCount >= 10 && fcReposoHoy.isDefined) {
        val rsFcAvg = conn.createStatement().executeQuery(
          "SELECT AVG(fc_reposo) as m FROM wellness WHERE fc_reposo IS NOT NULL AND fecha >= CURRENT_DATE - 30")
        val mediaFc = if (rsFcAvg.next()) rsFcAvg.getDouble("m") else 0.0
        val fcHoy = fcReposoHoy.get
        Some(
          if (fcHoy > mediaFc + 10) 2.0
          else if (fcHoy > mediaFc + 5) 4.0
          else if (fcHoy < mediaFc - 5) 10.0
          else 7.0
        )
      } else None

      val formaBase = fcScore match {
        case Some(fc) =>
          suenoScore * 0.20 + energiaScore * 0.20 + animoScore * 0.15 + acwrScore * 0.20 + descansoScore * 0.10 + phvScore * 0.05 + fc * 0.10
        case None =>
          suenoScore * 0.25 + energiaScore * 0.20 + animoScore * 0.15 + acwrScore * 0.25 + descansoScore * 0.10 + phvScore * 0.05
      }

      // BLOQUE A: deuda de sueno acumulada como factor negativo del indice de forma
      val deuda = calcularDeudaSueno()
      val deudaScore = formaDeudaScore(deuda)
      // BLOQUE C: carga cognitiva escolar (examenes/fin de trimestre) como factor negativo adicional
      val periodoEscolarHoy = periodoEscolarEnFecha(conn, LocalDate.now().toString)
      val cognitivoScore: Double = periodoEscolarHoy match {
        case Some("EXAMENES") | Some("TRIMESTRE_FIN") => -0.5
        case _ => 0.0
      }
      // BLOQUE F: somnolencia diurna (0-3) — sueno poco reparador aunque las horas cuadren
      val somnolenciaScore: Double = if (somnolenciaHoy.exists(_ >= 2)) -0.5 else 0.0
      val forma = Math.max(0.0, formaBase + deudaScore + cognitivoScore + somnolenciaScore)

      val upsert = conn.prepareStatement("""
        INSERT INTO forma_diaria (fecha, indice_forma, sueno_score, energia_score, animo_score, acwr_score, descanso_score, phv_score)
        VALUES (CURRENT_DATE, ?, ?, ?, ?, ?, ?, ?)
        ON CONFLICT (fecha) DO UPDATE SET indice_forma = EXCLUDED.indice_forma, sueno_score = EXCLUDED.sueno_score,
          energia_score = EXCLUDED.energia_score, animo_score = EXCLUDED.animo_score, acwr_score = EXCLUDED.acwr_score,
          descanso_score = EXCLUDED.descanso_score, phv_score = EXCLUDED.phv_score
      """)
      upsert.setDouble(1, forma); upsert.setDouble(2, suenoScore); upsert.setDouble(3, energiaScore)
      upsert.setDouble(4, animoScore); upsert.setDouble(5, acwrScore); upsert.setDouble(6, descansoScore); upsert.setDouble(7, phvScore)
      upsert.executeUpdate()

      Map(
        "indiceForma" -> forma,
        "suenoScore" -> suenoScore, "energiaScore" -> energiaScore, "animoScore" -> animoScore,
        "acwrScore" -> acwrScore, "descansoScore" -> descansoScore, "phvScore" -> phvScore,
        "fcScore" -> fcScore, "tieneFcHoy" -> fcReposoHoy.isDefined, "somnolenciaScore" -> somnolenciaScore,
        "deudaSueno" -> deuda
      )
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE D — UMBRALES DE ACWR SEGUN LA EDAD (los de referencia estan validados en adultos)
  // ─────────────────────────────────────────────────────────────────────────────
  // optimoMin-optimoMax: zona optima · > precaucion: vigilar · > riesgo: sobrecarga · > critico: riesgo alto.
  // Todas las clasificaciones de nivel/riesgo del ACWR pasan por aqui (no los agrupamientos estadisticos).
  case class UmbralesACWR(optimoMin: Double, optimoMax: Double, precaucion: Double, riesgo: Double, critico: Double)

  def umbralesACWR(): UmbralesACWR = {
    val edadAnios = try calcularEdadExacta(getLatestCardData().fechaNacimiento) catch { case _: Exception => 8 }
    if (edadAnios <= 8) UmbralesACWR(0.8, 1.1, 1.1, 1.3, 1.5)        // 6-8 años (mas conservadores)
    else if (edadAnios <= 12) UmbralesACWR(0.8, 1.2, 1.2, 1.4, 1.6)  // 9-12 años
    else UmbralesACWR(0.8, 1.3, 1.3, 1.5, 1.8)                       // adultos (referencia original)
  }

  /** (nivel, color bootstrap, etiqueta) del ACWR con los umbrales de la edad de Hector. */
  def nivelACWR(acwr: Double, u: UmbralesACWR = umbralesACWR()): (String, String, String) =
    if (acwr > u.critico) ("CRITICO", "danger", "RIESGO ALTO")
    else if (acwr > u.riesgo) ("RIESGO", "warning", "SOBRECARGA")
    else if (acwr > u.precaucion) ("PRECAUCION", "warning", "VIGILAR CARGA")
    else if (acwr < u.optimoMin) ("BAJA", "info", "BAJA CARGA")
    else ("OPTIMO", "success", "OPTIMO")

  val disclaimerACWR =
    "ℹ️ Los umbrales están adaptados para la franja de edad de Héctor. La investigación en menores de 8 años es limitada — usar como orientación, no como diagnóstico."

  // Componentes del Indice de Forma (0-10) — compartidos por calcularFormaHoy y predecirFormaPartido
  private def formaSuenoScore(profundoMin: Option[Int], horas: Double): Double = profundoMin match {
    case Some(m) if m > 90 => 10.0
    case Some(m) if m >= 60 => 7.0
    case Some(m) if m > 0 => 4.0
    case _ =>
      if (horas >= 9) 9.0
      else if (horas >= 7) 7.0
      else if (horas > 0) 4.0
      else 7.0
  }

  private def formaAcwrScore(acwr: Double, insuficiente: Boolean): Double = {
    val u = umbralesACWR()
    if (insuficiente) 7.0
    else if (acwr <= 0.0) 7.0
    else if (acwr < u.optimoMin) 6.0
    else if (acwr <= (u.optimoMin + u.optimoMax) / 2) 10.0
    else if (acwr <= u.optimoMax) 8.0
    else if (acwr <= u.riesgo) 5.0
    else 2.0
  }

  private def formaDescansoScore(diasDesdePartido: Option[Int]): Double = diasDesdePartido match {
    case Some(1) => 4.0
    case Some(2) => 7.0
    case Some(d) if d >= 3 && d <= 4 => 10.0
    case Some(d) if d >= 5 => 8.0
    case _ => 8.0
  }

  private def formaPhvScore(faseBio: String): Double =
    if (faseBio.contains("PICO ACTIVO")) 6.0
    else if (faseBio.contains("POST")) 9.0
    else if (faseBio.nonEmpty) 9.0
    else 8.0

  private def formaDeudaScore(deuda: Map[String, Any]): Double = {
    val deudaDias = deuda("diasConDatos").asInstanceOf[Int]
    val deudaHorasVal = deuda("deudaHoras").asInstanceOf[Double]
    if (deudaDias < 3) 0.0
    else if (deudaHorasVal < 2) 0.0
    else if (deudaHorasVal < 5) -0.5
    else if (deudaHorasVal < 8) -1.5
    else -2.5
  }

  def formaSemaforo(indice: Double): String = if (indice >= 7.5) "🟢" else if (indice >= 5.0) "🟡" else "🔴"

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE G — PREDICCION DEL INDICE DE FORMA PARA EL DIA DE PARTIDO (SQL/matematicas, sin Gemini)
  // ─────────────────────────────────────────────────────────────────────────────
  /**
   * ACWR proyectado al final del dia hoy+`diasAdelante`, asumiendo que se hacen todas las sesiones
   * de weekly_structure (incluidas las de hoy aun no registradas). Reutiliza proyectarACWR.
   * None si el historico es insuficiente para un ACWR fiable.
   */
  def acwrProyectadoHasta(diasAdelante: Int): Option[Double] = {
    val estado = calcularACWRConEstado()
    if (estado("status").asInstanceOf[String] == "INSUFICIENTE") return None
    if (diasAdelante <= 0) return Some(estado("acwr").asInstanceOf[Double])
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("SELECT dia_semana, tipo_sesion FROM weekly_structure WHERE activo = TRUE")
      var estructura = List[(Int, String)]()
      while (rs.next()) estructura = estructura :+ (rs.getInt("dia_semana"), rs.getString("tipo_sesion"))
      val hoy = LocalDate.now()
      def sesionesDe(d: LocalDate): List[String] = estructura.filter(_._1 == d.getDayOfWeek.getValue).map(_._2)
      // proyectarACWR admite una sesion por dia: se elige la de mas carga
      def principal(d: LocalDate): String = sesionesDe(d).sortBy(t => -cargaEstimadaSesion(t)).headOption.getOrElse("DESCANSO")
      val cargaHoyPendiente = sesionesDe(hoy).filterNot(t => sesionRegistrada(conn, hoy, t)).map(cargaEstimadaSesion).sum
      // proyectarACWR indexa los dias +1..+7 por nombre LUNES..DOMINGO en orden: el dia +k va en la posicion k-1
      val nombres = Seq("LUNES", "MARTES", "MIERCOLES", "JUEVES", "VIERNES", "SABADO", "DOMINGO")
      val k = math.min(diasAdelante, 7)
      val sesiones = (1 to k).map(i => nombres(i - 1) -> principal(hoy.plusDays(i))).toMap
      val proy = proyectarACWR(sesiones, cargaHoyPendiente)
      proy("dias").asInstanceOf[List[Map[String, Any]]].lift(k - 1).map(_("acwr").asInstanceOf[Double])
    } finally { conn.close() }
  }

  /** Indice de Forma estimado para dentro de `diasHastaPartido` dias: sueno/energia/animo = media de los 3 ultimos dias. */
  def predecirFormaPartido(diasHastaPartido: Int = 3): Map[String, Any] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("""
        SELECT AVG(horas_sueno) FILTER (WHERE horas_sueno > 0) as horas,
               AVG(sueno_profundo_min) FILTER (WHERE sueno_profundo_min > 0) as profundo,
               AVG(energia) as energia, AVG(animo) as animo, COUNT(*) as n
        FROM wellness WHERE fecha > CURRENT_DATE - 3""")
      rs.next()
      val n = rs.getInt("n")
      if (n == 0) return Map("disponible" -> false)
      val horas = rs.getDouble("horas")
      val profundo = Option(rs.getObject("profundo")).map(_ => math.round(rs.getDouble("profundo")).toInt)
      val energia = Option(rs.getObject("energia")).map(_ => rs.getDouble("energia")).getOrElse(3.0)
      val animo = Option(rs.getObject("animo")).map(_ => rs.getDouble("animo")).getOrElse(3.0)

      val fechaPartido = LocalDate.now().plusDays(diasHastaPartido)
      // Carga con la que se llega a la manana del partido: sesiones hasta el dia anterior
      val acwrProy = acwrProyectadoHasta(diasHastaPartido - 1)
      val rsUltimo = conn.createStatement().executeQuery(
        s"SELECT MAX(fecha) as f FROM matches WHERE status='PLAYED' ${seasonFilter(getTemporadaActivaId())}")
      val diasDescanso = if (rsUltimo.next() && rsUltimo.getDate("f") != null)
        Some(java.time.temporal.ChronoUnit.DAYS.between(rsUltimo.getDate("f").toLocalDate, fechaPartido).toInt) else None
      val faseBio = try getBioBandingData().getOrElse("faseBio", "").toString catch { case _: Exception => "" }

      val base = formaSuenoScore(profundo, horas) * 0.25 + energia * 2.0 * 0.20 + animo * 2.0 * 0.15 +
        formaAcwrScore(acwrProy.getOrElse(0.0), acwrProy.isEmpty) * 0.25 + formaDescansoScore(diasDescanso) * 0.10 +
        formaPhvScore(faseBio) * 0.05
      val indice = math.max(0.0, base + formaDeudaScore(calcularDeudaSueno()))
      Map("disponible" -> true, "indice" -> indice, "semaforo" -> formaSemaforo(indice),
        "acwrProyectado" -> acwrProy, "fechaPartido" -> fechaPartido.toString)
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE R — PREDICCION DE SOBRECARGA PROACTIVA (se usa el lunes: email y Telegram)
  // ─────────────────────────────────────────────────────────────────────────────
  /**
   * ACWR proyectado al final del sabado si Hector completa TODAS las sesiones de weekly_structure
   * de aqui al sabado (incluidas las de hoy y el partido). Alerta si supera 1.5.
   */
  def predecirSobrecargaSemana(): Option[String] = {
    val dow = LocalDate.now().getDayOfWeek.getValue
    if (dow > 5) return None
    val critico = umbralesACWR().critico
    acwrProyectadoHasta(6 - dow).filter(_ > critico).map { acwr =>
      f"⚠️ ALERTA DE CARGA: Si Héctor completa todas las sesiones previstas esta semana, llegará al partido del sábado con ACWR proyectado de $acwr%.2f (zona de riesgo). Considera reducir la intensidad del jueves o hablar con el entrenador."
    }
  }

  /** Dias hasta el sabado si hoy es miercoles o jueves y hay partido ese sabado (programado o en weekly_structure). */
  def diasHastaPartidoSabado(): Option[Int] = {
    val hoy = LocalDate.now()
    val dow = hoy.getDayOfWeek.getValue
    if (dow != 3 && dow != 4) return None
    val sabado = hoy.plusDays(6 - dow)
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("""
        SELECT (SELECT COUNT(*) FROM matches WHERE status='SCHEDULED' AND fecha::date = ?::date)
             + (SELECT COUNT(*) FROM weekly_structure WHERE activo = TRUE AND dia_semana = 6 AND tipo_sesion IN ('PARTIDO','TORNEO')) as c""")
      ps.setString(1, sabado.toString)
      val rs = ps.executeQuery()
      if (rs.next() && rs.getInt("c") > 0) Some(6 - dow) else None
    } finally { conn.close() }
  }

  // BLOQUE C1: hay partido programado hoy o mañana, O hoy coincide con el dia de PARTIDO de la
  // estructura semanal (weekly_structure) — nunca hardcodeado a sabado, siempre dinamico.
  def hayPartidoProximo(): Boolean = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT COUNT(*) as c FROM matches WHERE status='SCHEDULED' AND fecha >= CURRENT_DATE AND fecha <= CURRENT_DATE + 1")
      val hayProgramado = rs.next() && rs.getInt("c") > 0
      if (hayProgramado) true
      else {
        val diaSemana = LocalDate.now().getDayOfWeek.getValue
        val ps = conn.prepareStatement("SELECT COUNT(*) as c FROM weekly_structure WHERE tipo_sesion='PARTIDO' AND activo=TRUE AND dia_semana=?")
        ps.setInt(1, diaSemana)
        val rsW = ps.executeQuery()
        rsW.next() && rsW.getInt("c") > 0
      }
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE F — RIESGO DE LESION COMPUESTO (SQL puro, sin Gemini — solo tablas Elite)
  // ─────────────────────────────────────────────────────────────────────────────
  def calcularRiesgoLesion(): Map[String, Any] = {
    val conn = getConnection()
    try {
      // ACWR factor — FIX 2: sin penalizar cuando el historico es insuficiente (<3 semanas con datos)
      val acwrEstadoRiesgo = calcularACWRConEstado()
      val acwrInsuficienteRiesgo = acwrEstadoRiesgo("status").asInstanceOf[String] == "INSUFICIENTE"
      val acwr = acwrEstadoRiesgo("acwr").asInstanceOf[Double]
      val u = umbralesACWR()
      val (acwrFactor, hayAcwr) =
        if (acwrInsuficienteRiesgo) (0.0, false)
        else if (acwr < u.optimoMin) (1.0, true)
        else if (acwr <= u.optimoMax) (0.0, true)
        else if (acwr <= u.riesgo) (3.0, true)
        else (5.0, true)

      // PHV factor
      val faseBio = try getBioBandingData().getOrElse("faseBio", "").toString catch { case _: Exception => "" }
      val phvFactor =
        if (faseBio.contains("PICO ACTIVO")) 3.0
        else if (faseBio.contains("POST-PHV") || faseBio == "MADUREZ") 0.5
        else if (faseBio.nonEmpty) 0.5
        else 0.5

      // Descanso factor: dias consecutivos sin descanso (entreno o partido) terminando hoy
      val rsFechas = conn.createStatement().executeQuery("""
        (SELECT fecha FROM trainings WHERE fecha >= CURRENT_DATE - 14)
        UNION
        (SELECT fecha FROM matches WHERE status='PLAYED' AND fecha >= CURRENT_DATE - 14)
        ORDER BY fecha DESC
      """)
      var fechasActividad = Set[LocalDate]()
      while (rsFechas.next()) fechasActividad += rsFechas.getDate("fecha").toLocalDate
      var diasConsecutivos = 0
      var cursor = LocalDate.now()
      while (fechasActividad.contains(cursor)) { diasConsecutivos += 1; cursor = cursor.minusDays(1) }
      val descansoFactor =
        if (diasConsecutivos > 5) 2.0 else if (diasConsecutivos > 3) 1.0 else 0.0

      // FC reposo factor
      val rsFcHoy = conn.createStatement().executeQuery(
        "SELECT fc_reposo FROM wellness WHERE fecha >= CURRENT_DATE - 1 ORDER BY fecha DESC LIMIT 1")
      val fcHoy = if (rsFcHoy.next()) Option(rsFcHoy.getObject("fc_reposo")).map(_ => rsFcHoy.getInt("fc_reposo")) else None
      val fcFactor = fcHoy match {
        case Some(fc) =>
          val rsFcAvg = conn.createStatement().executeQuery(
            "SELECT AVG(fc_reposo) as m FROM wellness WHERE fc_reposo IS NOT NULL AND fecha >= CURRENT_DATE - 30")
          val media = if (rsFcAvg.next()) rsFcAvg.getDouble("m") else 0.0
          if (fc > media + 10) 2.0 else if (fc > media + 5) 1.0 else 0.0
        case None => 0.0
      }

      // Fatiga factor: media de energia (wellness) ultimos 3 dias
      val rsEnergia = conn.createStatement().executeQuery(
        "SELECT AVG(energia) as m FROM wellness WHERE fecha >= CURRENT_DATE - 3")
      val energiaMedia = if (rsEnergia.next()) rsEnergia.getDouble("m") else 0.0
      val fatigaFactor =
        if (energiaMedia > 0 && energiaMedia < 2.5) 1.5
        else if (energiaMedia > 0 && energiaMedia < 3.5) 0.5
        else 0.0

      // BLOQUE F: dolor muscular / agujetas de hoy (0-3)
      val rsDm = conn.createStatement().executeQuery(
        "SELECT dolor_muscular FROM wellness WHERE fecha >= CURRENT_DATE - 1 AND dolor_muscular IS NOT NULL ORDER BY fecha DESC LIMIT 1")
      val dolorMuscular = if (rsDm.next()) rsDm.getInt("dolor_muscular") else 0
      val dolorMuscularFactor = if (dolorMuscular >= 3) 1.5 else if (dolorMuscular >= 2) 0.5 else 0.0

      val riesgo = math.min(10.0, acwrFactor + phvFactor + descansoFactor + fcFactor + fatigaFactor + dolorMuscularFactor)
      val (clasificacion, semaforo) =
        if (riesgo < 2.0) ("BAJO", "🟢")
        else if (riesgo < 4.0) ("MEDIO", "🟡")
        else if (riesgo < 6.0) ("ALTO", "🟠")
        else ("CRITICO", "🔴")

      val factoresActivos = scala.collection.mutable.ListBuffer[String]()
      if (acwrFactor > 0) factoresActivos += s"ACWR ${"%.2f".format(acwr)}"
      if (phvFactor >= 3.0) factoresActivos += "Pico de crecimiento (PHV)"
      if (descansoFactor > 0) factoresActivos += s"$diasConsecutivos días seguidos sin descanso"
      if (fcFactor > 0) factoresActivos += "FC en reposo elevada"
      if (fatigaFactor > 0) factoresActivos += "Energía baja en los últimos días"
      if (dolorMuscularFactor > 0) factoresActivos += (if (dolorMuscular >= 3) "Dolor muscular fuerte" else "Dolor muscular moderado")

      Map(
        "riesgo" -> riesgo, "clasificacion" -> clasificacion, "semaforo" -> semaforo,
        "acwrFactor" -> acwrFactor, "phvFactor" -> phvFactor, "descansoFactor" -> descansoFactor,
        "fcFactor" -> fcFactor, "fatigaFactor" -> fatigaFactor, "dolorMuscularFactor" -> dolorMuscularFactor, "factoresActivos" -> factoresActivos.toList
      )
    } finally { conn.close() }
  }

  // BLOQUE G3: evita reenviar la alerta de Telegram de riesgo CRITICO mas de una vez al dia
  def yaAlertadoRiesgoCriticoHoy(): Boolean = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT COUNT(*) as c FROM feature_cache WHERE cache_key = 'telegram_riesgo_critico' AND updated_at::date = CURRENT_DATE")
      rs.next() && rs.getInt("c") > 0
    } finally { conn.close() }
  }

  def marcarRiesgoCriticoAlertadoHoy(): Unit = {
    val conn = getConnection()
    try {
      conn.createStatement().executeUpdate(
        "INSERT INTO feature_cache (cache_key, payload, updated_at) VALUES ('telegram_riesgo_critico', '1', NOW()) ON CONFLICT (cache_key) DO UPDATE SET payload='1', updated_at=NOW()")
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE C — MODO DIA DE PARTIDO (SQL puro, sin Gemini — solo tablas Elite)
  // ─────────────────────────────────────────────────────────────────────────────
  def getDiaPartidoInfo(): Map[String, Any] = {
    val conn = getConnection()
    try {
      val rsM = conn.createStatement().executeQuery(
        "SELECT id, rival, tipo_partido, torneo_nombre, fase FROM matches WHERE status='SCHEDULED' AND fecha >= CURRENT_DATE AND fecha <= CURRENT_DATE + 1 ORDER BY fecha ASC LIMIT 1")
      if (!rsM.next()) return Map("hayRivalProgramado" -> false)

      val scheduleId = rsM.getInt("id")
      val rival = fixEncoding(Option(rsM.getString("rival")).getOrElse(""))
      val tipoPartido = Option(rsM.getString("tipo_partido")).getOrElse("")
      val torneoNombre = fixEncoding(Option(rsM.getString("torneo_nombre")).getOrElse(""))
      val fase = fixEncoding(Option(rsM.getString("fase")).getOrElse(""))

      val rsH = conn.prepareStatement(
        "SELECT COUNT(*) as pj, COALESCE(AVG(nota),0) as media FROM matches WHERE LOWER(rival) LIKE LOWER(?) AND status='PLAYED'")
      rsH.setString(1, s"%$rival%")
      val rsHr = rsH.executeQuery()
      rsHr.next()
      val pj = rsHr.getInt("pj"); val notaMedia = rsHr.getDouble("media")

      val rsU = conn.prepareStatement(
        "SELECT goles_favor, goles_contra FROM matches WHERE LOWER(rival) LIKE LOWER(?) AND status='PLAYED' ORDER BY fecha DESC LIMIT 1")
      rsU.setString(1, s"%$rival%")
      val rsUr = rsU.executeQuery()
      val ultimoResultado = if (rsUr.next()) s"${rsUr.getInt("goles_favor")}-${rsUr.getInt("goles_contra")}" else ""

      val estilo = getRivalInfo(rival).map(_.estilo).filter(_.nonEmpty)
      val arquetipo = getStrikerClusters()
        .find(c => c("rival").asInstanceOf[String].toLowerCase.contains(rival.toLowerCase))
        .map(_("arquetipo").asInstanceOf[String])

      Map(
        "hayRivalProgramado" -> true, "scheduleId" -> scheduleId, "rival" -> rival,
        "tipoPartido" -> tipoPartido, "torneoNombre" -> torneoNombre, "fase" -> fase,
        "pj" -> pj, "notaMedia" -> notaMedia, "ultimoResultado" -> ultimoResultado,
        "estilo" -> estilo, "arquetipo" -> arquetipo
      )
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE D — CLIMA AUTOMATICO (Open-Meteo) — solo para /match-center de Hector
  // ─────────────────────────────────────────────────────────────────────────────
  def getClimaParaFecha(fecha: String, lat: Double = 40.4168, lon: Double = -3.7038): String = {
    try {
      // El endpoint forecast solo cubre ~3 meses hacia atras; para fechas antiguas (BLOQUE C, relleno
      // historico) se usa el archivo historico, que acepta los mismos parametros daily.
      val antigua = scala.util.Try(LocalDate.parse(fecha).isBefore(LocalDate.now().minusDays(60))).getOrElse(false)
      val base = if (antigua) "https://archive-api.open-meteo.com/v1/archive" else "https://api.open-meteo.com/v1/forecast"
      val url = s"$base?latitude=$lat&longitude=$lon&daily=precipitation_sum,weathercode,temperature_2m_max,windspeed_10m_max&start_date=$fecha&end_date=$fecha&timezone=Europe/Madrid"
      val r = requests.get(url, readTimeout = 3000, connectTimeout = 3000)
      if (r.statusCode != 200) return ""
      val json = ujson.read(r.text())
      val daily = json("daily")
      val weathercode = daily("weathercode")(0).num.toInt
      val tempMax = daily("temperature_2m_max")(0).num
      val windMax = daily("windspeed_10m_max")(0).num

      if (tempMax > 28) "Calor"
      else if (tempMax < 8) "Frio"
      else if (windMax > 30) "Viento"
      else if (weathercode == 0) "Sol"
      else if (Seq(1,2,3).contains(weathercode)) "Nubes"
      else if ((51 to 67).contains(weathercode) || (71 to 77).contains(weathercode) || (80 to 82).contains(weathercode)) "Lluvia"
      else ""
    } catch { case _: Exception => "" }
  }

  // BLOQUE G3: aviso Telegram del dia de partido a las 9:00 AM — dia siempre dinamico via weekly_structure
  def getAvisoDiaPartido(): Option[String] = {
    val conn = getConnection()
    try {
      val diaSemana = LocalDate.now().getDayOfWeek.getValue
      val rsW = conn.prepareStatement("SELECT COUNT(*) as c FROM weekly_structure WHERE tipo_sesion='PARTIDO' AND activo=TRUE AND dia_semana=?")
      rsW.setInt(1, diaSemana)
      val rsWr = rsW.executeQuery()
      val esDiaPartido = rsWr.next() && rsWr.getInt("c") > 0
      if (!esDiaPartido) return None

      val rsM = conn.createStatement().executeQuery(
        "SELECT rival, tipo_partido, torneo_nombre, fase FROM matches WHERE status='SCHEDULED' AND fecha BETWEEN CURRENT_DATE AND CURRENT_DATE + INTERVAL '2 days' ORDER BY fecha ASC LIMIT 1")
      val rivalTxt = if (rsM.next()) {
        val rival = fixEncoding(Option(rsM.getString("rival")).getOrElse(""))
        val tipoPartido = Option(rsM.getString("tipo_partido")).getOrElse("")
        val torneoNombre = fixEncoding(Option(rsM.getString("torneo_nombre")).getOrElse(""))
        val fase = fixEncoding(Option(rsM.getString("fase")).getOrElse(""))
        val torneoTxt = if (tipoPartido == "TORNEO" && torneoNombre.nonEmpty) s" ($torneoNombre${if (fase.nonEmpty) s" — $fase" else ""})" else ""
        s" vs $rival$torneoTxt"
      } else ""

      val formaHoy = calcularFormaHoy()
      val indice = formaHoy("indiceForma").asInstanceOf[Double]
      val semaforo = if (indice >= 7.5) "🟢" else if (indice >= 5.0) "🟡" else "🔴"

      Some(s"🏟️ HOY ES DÍA DE PARTIDO$rivalTxt · Índice de Forma: ${"%.1f".format(indice)} $semaforo")
    } finally { conn.close() }
  }

  // BLOQUE C3: partido de hoy ya registrado como jugado (para el aviso post-partido)
  def getPartidoHoyRegistrado(): Option[Int] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT id FROM matches WHERE status='PLAYED' AND fecha = CURRENT_DATE ORDER BY id DESC LIMIT 1")
      if (rs.next()) Some(rs.getInt("id")) else None
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE 5.6 — DETECTOR DE DESGASTE SILENCIOSO (SQL puro, sin Gemini)
  // ─────────────────────────────────────────────────────────────────────────────
  def detectarDesgasteSilencioso(): Option[String] = {
    val conn = getConnection()
    try {
      var indicadores = 0

      val rsPsych = conn.createStatement().executeQuery(
        "SELECT motivacion, disfrute FROM psych_records WHERE fecha >= CURRENT_DATE - 21 ORDER BY fecha DESC LIMIT 3")
      var bajos = 0; var total = 0
      while (rsPsych.next()) { total += 1; if (rsPsych.getInt("motivacion") < 3 || rsPsych.getInt("disfrute") < 3) bajos += 1 }
      if (total >= 2 && bajos >= 2) indicadores += 1

      val rsEnergia = conn.createStatement().executeQuery(
        "SELECT AVG(energia) as m FROM wellness WHERE fecha >= CURRENT_DATE - 14 AND energia IS NOT NULL")
      if (rsEnergia.next() && rsEnergia.getDouble("m") > 0 && rsEnergia.getDouble("m") < 3) indicadores += 1

      val rsVoz = conn.createStatement().executeQuery("""
        SELECT COUNT(*) as c FROM matches
        WHERE fecha >= CURRENT_DATE - 21 AND analisis_voz IS NOT NULL
        AND (analisis_voz ILIKE '%cansado%' OR analisis_voz ILIKE '%aburrido%' OR analisis_voz ILIKE '%no quiero%')""")
      if (rsVoz.next() && rsVoz.getInt("c") > 0) indicadores += 1

      // MODULO LA VOZ DEL PORTERO: carita de los ultimos 2 meses <= 2, cuenta como señal adicional
      val rsVozPortero = conn.createStatement().executeQuery(
        "SELECT motivacion_carita FROM voz_portero ORDER BY fecha DESC LIMIT 2")
      var vozBajaCount = 0; var vozTotal = 0
      while (rsVozPortero.next()) { vozTotal += 1; if (rsVozPortero.getInt("motivacion_carita") <= 2) vozBajaCount += 1 }
      if (vozTotal >= 2 && vozBajaCount >= 2) indicadores += 1

      if (indicadores >= 2)
        Some("⚠️ DESCOMPRESIÓN RECOMENDADA — Varios indicadores sugieren saturación silenciosa. Esta semana: cero correcciones técnicas, cero conversaciones de fútbol en casa. Tiempo libre, juego sin agenda. Vuelve al modo normal la semana siguiente.")
      else None
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE 5.4 — MICRO-OBJETIVOS SEMANALES (SQL puro, sin Gemini)
  // ─────────────────────────────────────────────────────────────────────────────
  private def lunesEstaSemana(): String = java.time.LocalDate.now().`with`(java.time.DayOfWeek.MONDAY).toString

  def getMicroObjetivoSemana(): Map[String, Any] = {
    val conn = getConnection()
    try {
      val lunes = lunesEstaSemana()
      val ps = conn.prepareStatement("SELECT * FROM micro_objetivos WHERE semana_inicio = ?::date")
      ps.setString(1, lunes)
      val rs = ps.executeQuery()
      if (rs.next()) {
        Map("id" -> rs.getInt("id"), "semanaInicio" -> rs.getDate("semana_inicio").toString,
          "objetivo" -> rs.getString("objetivo_semana"), "dimension" -> Option(rs.getString("dimension_idp")).getOrElse(""),
          "completado" -> rs.getBoolean("completado"), "resultado" -> Option(rs.getString("resultado")).getOrElse(""),
          "generadoIA" -> rs.getBoolean("generado_ia"))
      } else {
        val rsIdp = conn.createStatement().executeQuery(
          "SELECT dimension, objetivo FROM idp_objetivos WHERE progreso_pct < 100 ORDER BY progreso_pct ASC LIMIT 1")
        val (dim, objetivoTxt) = if (rsIdp.next()) (rsIdp.getString("dimension"), rsIdp.getString("objetivo")) else ("General", "Mantén la constancia en el entrenamiento esta semana.")

        val rsRub = conn.createStatement().executeQuery("""
          SELECT rubrica_posicion, rubrica_decisiones, rubrica_pies, rubrica_comunicacion, rubrica_actitud
          FROM matches WHERE status='PLAYED' AND rubrica_posicion IS NOT NULL ORDER BY fecha DESC LIMIT 1""")
        val dimRubrica = if (rsRub.next()) {
          val dims = Seq("Posición" -> rsRub.getInt("rubrica_posicion"), "Decisiones" -> rsRub.getInt("rubrica_decisiones"),
            "Pies" -> rsRub.getInt("rubrica_pies"), "Comunicación" -> rsRub.getInt("rubrica_comunicacion"),
            "Actitud" -> rsRub.getInt("rubrica_actitud"))
          Some(dims.minBy(_._2)._1)
        } else None

        val objetivoSemana = s"Trabajar: $objetivoTxt" + dimRubrica.map(d => s" (foco rúbrica: $d)").getOrElse("")
        val ins = conn.prepareStatement(
          "INSERT INTO micro_objetivos (semana_inicio, objetivo_semana, dimension_idp, generado_ia) VALUES (?::date,?,?,TRUE) ON CONFLICT (semana_inicio) DO NOTHING")
        ins.setString(1, lunes); ins.setString(2, objetivoSemana); ins.setString(3, dim)
        ins.executeUpdate()
        Map("id" -> 0, "semanaInicio" -> lunes, "objetivo" -> objetivoSemana, "dimension" -> dim,
          "completado" -> false, "resultado" -> "", "generadoIA" -> true)
      }
    } finally { conn.close() }
  }

  def completarMicroObjetivo(resultado: String): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement(
        "UPDATE micro_objetivos SET completado=TRUE, resultado=? WHERE semana_inicio = ?::date")
      ps.setString(1, fixEncoding(resultado)); ps.setString(2, lunesEstaSemana())
      ps.executeUpdate()
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE 5.5 — PREPARACION SEMANAL IA
  // ─────────────────────────────────────────────────────────────────────────────
  private def proximoRivalYHash(conn: Connection): Option[(String, String, String)] = {
    val rsProx = conn.createStatement().executeQuery(
      "SELECT rival, fecha FROM matches WHERE status='SCHEDULED' ORDER BY fecha ASC LIMIT 1")
    if (!rsProx.next()) None
    else {
      val rival = rsProx.getString("rival"); val fechaProx = rsProx.getDate("fecha").toString
      val semanaAnio = java.time.LocalDate.now().get(java.time.temporal.WeekFields.ISO.weekOfWeekBasedYear())
      Some((rival, fechaProx, s"${s"$rival-$semanaAnio".hashCode}"))
    }
  }

  /** Solo lectura de cache — NUNCA llama a Gemini. Para usar en el render del dashboard. */
  def getPreparacionSemanalCache(): Option[String] = {
    val conn = getConnection()
    try {
      proximoRivalYHash(conn).flatMap { case (_, _, hash) =>
        val cached = conn.prepareStatement(
          "SELECT payload FROM feature_cache WHERE cache_key = ? AND updated_at > NOW() - INTERVAL '7 days'")
        cached.setString(1, s"preparacion_semanal_$hash")
        val rsCache = cached.executeQuery()
        if (rsCache.next()) Some(rsCache.getString("payload")) else None
      }
    } finally { conn.close() }
  }

  /** Llama a Gemini — SOLO desde el boton explicito "Generar preparacion semanal". */
  def generarPreparacionSemanal(): String = {
    val conn = getConnection()
    try {
      val card = getLatestCardData(); val edad = calcularEdadExacta(card.fechaNacimiento)
      val (rival, fechaProx, hash) = proximoRivalYHash(conn).getOrElse(return "")

      val rsAc = conn.prepareStatement("SELECT COALESCE(SUM(rpe*60),0) FROM trainings WHERE fecha >= CURRENT_DATE - ?")
      rsAc.setInt(1, 7); val ac = { val r = rsAc.executeQuery(); if (r.next()) r.getDouble(1) else 0.0 }
      val rsCh = conn.prepareStatement("SELECT COALESCE(SUM(rpe*60),0) / 4.0 FROM trainings WHERE fecha >= CURRENT_DATE - ?")
      rsCh.setInt(1, 28); val ch = { val r = rsCh.executeQuery(); if (r.next()) r.getDouble(1) else 0.0 }
      val acwr = if (ch > 0) ac / ch else 1.0
      val estadoAcwr = if (acwr < 0.8) "BAJA" else if (acwr <= 1.2) "NORMAL" else "ALTA"

      val fallos = getTechnicalAlerts().take(3).mkString("; ")

      // BLOQUE B3: zona vulnerable calculada solo con partidos de la temporada activa
      val rsZona = conn.createStatement().executeQuery(
        s"SELECT zona_goles FROM matches WHERE status='PLAYED' AND zona_goles IS NOT NULL AND zona_goles != '' ${seasonFilter(getTemporadaActivaId())} ORDER BY fecha DESC LIMIT 10")
      val zonaCounts = scala.collection.mutable.Map[String, Int]().withDefaultValue(0)
      while (rsZona.next()) rsZona.getString("zona_goles").split(",").filter(_.nonEmpty).foreach(z => zonaCounts(z.trim) += 1)
      val zonaVulnerable = if (zonaCounts.nonEmpty) zonaCounts.maxBy(_._2)._1 else "sin datos suficientes"

      // BLOQUE E3: si hay un analisis de video reciente, se incorpora como contexto adicional
      val errorVideoLinea = getUltimoErrorRecurrente() match {
        case Some(error) => s" El último análisis de vídeo detectó este error técnico recurrente en Héctor: $error. Incorpóralo en la CONSIGNA_COCHE o en el FOCO_SABADO si es relevante."
        case None => ""
      }

      // BLOQUE A: deuda de sueno — solo si es relevante (MODERADA o superior)
      val deudaSem = calcularDeudaSueno()
      val deudaNivelSem = deudaSem("nivel").asInstanceOf[String]
      val deudaLinea =
        if (deudaNivelSem == "MINIMA") ""
        else s" Deuda de sueño acumulada esta semana: ${"%.1f".format(deudaSem("deudaHoras").asInstanceOf[Double])}h (nivel $deudaNivelSem, media ${"%.1f".format(deudaSem("mediaDiaria").asInstanceOf[Double])}h/noche). Ten esto en cuenta en el DESCANSO_CASA."

      // BLOQUE I: firma de fatiga personal — solo si el ACWR es alto y hay datos suficientes
      val firmaFatigaLinea = if (estadoAcwr != "ALTA") "" else {
        val firma = getFirmaFatiga()
        if (firma("suficiente").asInstanceOf[Boolean]) {
          val dimension = firma("firmaFatiga").asInstanceOf[String]
          s" Basado en el historial de Héctor, cuando llega cargado su $dimension tiende a bajar primero. El foco de observación del sábado debería ser precisamente eso — inclúyelo en el FOCO_SABADO."
        } else ""
      }

      // BLOQUE C: carga cognitiva escolar (examenes/fin de trimestre)
      val cargaEscolarLinea = getPeriodoEscolarHoy() match {
        case Some("EXAMENES") | Some("TRIMESTRE_FIN") =>
          " Esta semana coincide con período de exámenes escolares — la bajada de energía o rendimiento puede tener origen escolar, no deportivo. Ten esto en cuenta en el tono del mensaje."
        case _ => ""
      }

      val prompt = s"""Eres el preparador de Héctor, portero de $edad años. Esta semana: próximo partido vs $rival el $fechaProx, ACWR=${"%.2f".format(acwr)} ($estadoAcwr), fallos recurrentes=${if (fallos.nonEmpty) fallos else "ninguno detectado"}, zona vulnerable=$zonaVulnerable.$errorVideoLinea$deudaLinea$firmaFatigaLinea$cargaEscolarLinea Genera exactamente 3 bloques en texto plano: CONSIGNA_COCHE: [un foco mental o técnico positivo, máximo 2 frases, sin presión, para decirle de camino al entreno] / DESCANSO_CASA: [una pauta concreta de recuperación en casa según el ACWR, una frase accionable] / FOCO_SABADO: [un solo aspecto técnico que el padre debe observar desde la grada el sábado como observador neutral, máximo 2 frases]. Lenguaje para un padre, sin tecnicismos."""

      val resultado = AIProvider.ask(prompt, None, bypassCache = true)
      val up = conn.prepareStatement(
        "INSERT INTO feature_cache (cache_key, payload, updated_at) VALUES (?,?,NOW()) ON CONFLICT (cache_key) DO UPDATE SET payload=EXCLUDED.payload, updated_at=NOW()")
      up.setString(1, s"preparacion_semanal_$hash"); up.setString(2, resultado)
      up.executeUpdate()
      resultado
    } finally { conn.close() }
  }

  // ── BLOQUE A4: validacion del indice de forma frente al rendimiento real ────
  def getFormaCorrelacion(): Map[String, Any] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT indice_forma, nota_partido FROM forma_diaria WHERE nota_partido IS NOT NULL ORDER BY fecha ASC")
      var puntos = List[(Double, Double)]()
      while (rs.next()) puntos = puntos :+ (rs.getDouble("indice_forma"), rs.getDouble("nota_partido"))

      val n = puntos.size
      if (n < 10) return Map("suficiente" -> false, "n" -> n, "puntos" -> List.empty[Map[String, Double]])

      val xs = puntos.map(_._1); val ys = puntos.map(_._2)
      val mediaX = xs.sum / n; val mediaY = ys.sum / n
      val cov = xs.zip(ys).map { case (x, y) => (x - mediaX) * (y - mediaY) }.sum
      val sdX = math.sqrt(xs.map(x => math.pow(x - mediaX, 2)).sum)
      val sdY = math.sqrt(ys.map(y => math.pow(y - mediaY, 2)).sum)
      val r = if (sdX > 0 && sdY > 0) cov / (sdX * sdY) else 0.0

      // Regresion lineal simple para la linea de tendencia
      val varX = xs.map(x => math.pow(x - mediaX, 2)).sum
      val pendiente = if (varX > 0) cov / varX else 0.0
      val intercepto = mediaY - pendiente * mediaX

      val altaForma = puntos.filter(_._1 >= 8).map(_._2)
      val bajaForma = puntos.filter(_._1 < 6).map(_._2)
      val notaMediaAlta = if (altaForma.nonEmpty) altaForma.sum / altaForma.size else 0.0
      val notaMediaBaja = if (bajaForma.nonEmpty) bajaForma.sum / bajaForma.size else 0.0

      Map(
        "suficiente" -> true, "n" -> n,
        "puntos" -> puntos.map { case (x, y) => Map("x" -> x, "y" -> y) },
        "r" -> r, "pendiente" -> pendiente, "intercepto" -> intercepto,
        "notaMediaAlta" -> notaMediaAlta, "notaMediaBaja" -> notaMediaBaja
      )
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE B — INDICE DE COGNICION ANTICIPATORIA
  // ─────────────────────────────────────────────────────────────────────────────
  private def calcularIndiceCognitivo(reaccionAciertos: Int, lecturaSenales: Int, velocidadDecision: Int, pausaCognitiva: Int): Double =
    (reaccionAciertos * 10) * 0.30 + (lecturaSenales * 20) * 0.25 + (velocidadDecision * 20) * 0.25 + (pausaCognitiva * 20) * 0.20

  // Guardado del test — SQL puro, sin Gemini
  def saveCognitivoTest(fecha: String, reaccionAciertos: Int, reaccionTotal: Int, lecturaSenales: Int,
                         velocidadDecision: Int, pausaCognitiva: Int, notas: String): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("""
        INSERT INTO cognitivo_tests (fecha, reaccion_aciertos, reaccion_total, lectura_senales, velocidad_decision, pausa_cognitiva, notas)
        VALUES (?::date, ?, ?, ?, ?, ?, ?)
      """)
      ps.setString(1, if (fecha.nonEmpty) fecha else LocalDate.now().toString)
      ps.setInt(2, reaccionAciertos); ps.setInt(3, reaccionTotal); ps.setInt(4, lecturaSenales)
      ps.setInt(5, velocidadDecision); ps.setInt(6, pausaCognitiva); ps.setString(7, fixEncoding(notas))
      ps.executeUpdate()
    } finally { conn.close() }
  }

  // Lectura y calculo del indice — SQL/matematicas puras, sin Gemini
  def getCognitivoTests(): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("SELECT * FROM cognitivo_tests ORDER BY fecha ASC")
      var list = List[Map[String, Any]]()
      while (rs.next()) {
        val aciertos = rs.getInt("reaccion_aciertos")
        val total = rs.getInt("reaccion_total")
        val lectura = rs.getInt("lectura_senales")
        val velocidad = rs.getInt("velocidad_decision")
        val pausa = rs.getInt("pausa_cognitiva")
        val indice = calcularIndiceCognitivo(aciertos, lectura, velocidad, pausa)
        list = list :+ Map[String, Any](
          "id" -> rs.getInt("id"), "fecha" -> rs.getDate("fecha").toString,
          "reaccionAciertos" -> aciertos, "reaccionTotal" -> total,
          "lecturaSenales" -> lectura, "velocidadDecision" -> velocidad, "pausaCognitiva" -> pausa,
          "notas" -> fixEncoding(Option(rs.getString("notas")).getOrElse("")),
          "indice" -> indice
        )
      }
      list
    } finally { conn.close() }
  }

  // Lectura desde cache unicamente — nunca llama a Gemini en el render de pagina
  def getCognitivoAnalysisCached(): Option[String] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT payload FROM feature_cache WHERE cache_key = 'cognitivo_analysis' AND updated_at > NOW() - INTERVAL '30 days'")
      if (rs.next()) Some(ujson.read(rs.getString("payload"))("analisis").str) else None
    } finally { conn.close() }
  }

  // Llamada real a Gemini — SOLO se invoca desde el boton POST "Analisis IA"
  def generateCognitivoAnalysis(): String = {
    val conn = getConnection()
    try {
      val tests = getCognitivoTests()
      if (tests.isEmpty) return "Sin tests cognitivos registrados todavía."
      val card = getLatestCardData()
      val edad = calcularEdadExacta(card.fechaNacimiento)
      val historicoStr = tests.map { t =>
        val fecha = t("fecha").asInstanceOf[String]
        val indice = t("indice").asInstanceOf[Double]
        s"$fecha: índice=${f"$indice%.0f"} (reacción=${t("reaccionAciertos")}/${t("reaccionTotal")}, lectura=${t("lecturaSenales")}/5, decisión=${t("velocidadDecision")}/5, pausa=${t("pausaCognitiva")}/5)"
      }.mkString("; ")

      val prompt = s"""Héctor es un portero de $edad años. Sus resultados cognitivos trimestrales son: $historicoStr. Los porteros de élite toman decisiones en 240ms vs 300ms de los novatos. Evalúa: 1) Si el perfil cognitivo de Héctor muestra rasgos de anticipación avanzada para su edad, 2) Qué componente cognitivo es su punto diferenciador, 3) Un ejercicio concreto de academia para mejorar la lectura de señales corporales. Máximo 3 líneas por punto."""

      val analisis = AIProvider.ask(prompt, None, bypassCache = true)
      val payload = ujson.Obj("analisis" -> analisis)
      val upsert = conn.prepareStatement("""
        INSERT INTO feature_cache (cache_key, payload, updated_at) VALUES ('cognitivo_analysis', ?, NOW())
        ON CONFLICT (cache_key) DO UPDATE SET payload = EXCLUDED.payload, updated_at = NOW()
      """)
      upsert.setString(1, ujson.write(payload))
      upsert.executeUpdate()
      analisis
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE C — URL PUBLICA CONTROLADA DE HECTOR
  // ─────────────────────────────────────────────────────────────────────────────
  def getPerfilPublicoConfig(): Map[String, Any] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("SELECT * FROM perfil_publico ORDER BY id ASC LIMIT 1")
      if (rs.next()) Map(
        "id" -> rs.getInt("id"),
        "activo" -> rs.getBoolean("activo"),
        "passwordLectura" -> Option(rs.getString("password_lectura")).getOrElse(""),
        "mostrarCarta" -> rs.getBoolean("mostrar_carta"),
        "mostrarProgresion" -> rs.getBoolean("mostrar_progresion"),
        "mostrarVideoIa" -> rs.getBoolean("mostrar_video_ia"),
        "mostrarIdp" -> rs.getBoolean("mostrar_idp"),
        "mostrarInforme" -> rs.getBoolean("mostrar_informe"),
        "mostrarCognitivo" -> rs.getBoolean("mostrar_cognitivo"),
        "mostrarMedico" -> rs.getBoolean("mostrar_medico"),
        "mostrarArquetipo" -> rs.getBoolean("mostrar_arquetipo"),
        "mostrarVozPortero" -> rs.getBoolean("mostrar_voz_portero"),
        "visitas" -> rs.getInt("visitas"),
        "ultimaVisita" -> Option(rs.getTimestamp("ultima_visita")).map(_.toString).getOrElse("")
      ) else Map(
        "activo" -> false, "passwordLectura" -> "", "mostrarCarta" -> true, "mostrarProgresion" -> true,
        "mostrarVideoIa" -> true, "mostrarIdp" -> true, "mostrarInforme" -> true, "mostrarCognitivo" -> false,
        "mostrarMedico" -> false, "mostrarArquetipo" -> true, "mostrarVozPortero" -> false, "visitas" -> 0, "ultimaVisita" -> ""
      )
    } finally { conn.close() }
  }

  def updatePerfilPublicoConfig(activo: Boolean, password: String, mostrarCarta: Boolean, mostrarProgresion: Boolean,
                                 mostrarVideoIa: Boolean, mostrarIdp: Boolean, mostrarInforme: Boolean,
                                 mostrarCognitivo: Boolean, mostrarMedico: Boolean, mostrarArquetipo: Boolean = true,
                                 mostrarVozPortero: Boolean = false): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("""
        UPDATE perfil_publico SET activo = ?, mostrar_carta = ?, mostrar_progresion = ?, mostrar_video_ia = ?,
          mostrar_idp = ?, mostrar_informe = ?, mostrar_cognitivo = ?, mostrar_medico = ?, mostrar_arquetipo = ?,
          mostrar_voz_portero = ?
        WHERE id = (SELECT MIN(id) FROM perfil_publico)
      """)
      ps.setBoolean(1, activo); ps.setBoolean(2, mostrarCarta); ps.setBoolean(3, mostrarProgresion)
      ps.setBoolean(4, mostrarVideoIa); ps.setBoolean(5, mostrarIdp); ps.setBoolean(6, mostrarInforme)
      ps.setBoolean(7, mostrarCognitivo); ps.setBoolean(8, mostrarMedico); ps.setBoolean(9, mostrarArquetipo)
      ps.setBoolean(10, mostrarVozPortero)
      ps.executeUpdate()
      if (password.nonEmpty) {
        val psPass = conn.prepareStatement("UPDATE perfil_publico SET password_lectura = ? WHERE id = (SELECT MIN(id) FROM perfil_publico)")
        psPass.setString(1, password)
        psPass.executeUpdate()
      }
    } finally { conn.close() }
  }

  def validatePublicPassword(pass: String): Boolean = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("SELECT password_lectura, activo FROM perfil_publico ORDER BY id ASC LIMIT 1")
      if (rs.next()) {
        val stored = Option(rs.getString("password_lectura")).getOrElse("")
        rs.getBoolean("activo") && stored.nonEmpty && pass.nonEmpty && stored == pass
      } else false
    } finally { conn.close() }
  }

  def registrarVisitaPublica(): Unit = {
    val conn = getConnection()
    try {
      conn.createStatement().executeUpdate(
        "UPDATE perfil_publico SET visitas = visitas + 1, ultima_visita = NOW() WHERE id = (SELECT MIN(id) FROM perfil_publico)")
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // MODULO — SISTEMA DE BACKUPS AUTOMATICOS
  // ─────────────────────────────────────────────────────────────────────────────
  // Genera un dump SQL insertable con todas las tablas de datos de Hector.
  // Calculo/lectura de BD puro — sin Gemini, seguro de invocar desde un boton o desde el hilo de backup.
  def generarBackupSQL(): String = {
    val conn = getConnection()
    try {
      val sb = new StringBuilder
      sb.append(s"-- Guardian Elite Backup\n")
      sb.append(s"-- Generado: ${java.time.LocalDateTime.now()}\n")
      sb.append(s"-- Hector Martin Gonzalez -- Portero\n\n")

      // Lista de tablas reales del esquema (ver initDB) — excluye cachés regenerables
      // (ai_cache, feature_cache) y la propia tabla de backups.
      val tablas = List(
        "seasons", "matches", "wellness", "match_goals", "growth_history",
        "medical_records", "trainings", "drills", "technical_reviews", "gear",
        "injuries", "rivals", "penalties", "video_tags", "objectives",
        "academic_performance", "legends_milestones", "scouting_reports",
        "nutrition_plans", "footbar_sessions", "goalkeeper_skills",
        "development_windows", "opportunities", "contacts", "visibility_events",
        "season_diary", "periodization", "psych_records", "physical_tests",
        "idp_temporadas", "idp_objetivos", "idp_revisiones",
        "forma_diaria", "cognitivo_tests", "perfil_publico"
      )

      tablas.foreach { tabla =>
        try {
          val rs = conn.createStatement().executeQuery(s"SELECT * FROM $tabla ORDER BY id")
          val meta = rs.getMetaData
          val cols = (1 to meta.getColumnCount).map(meta.getColumnName)
          sb.append(s"\n-- TABLA: $tabla\n")
          while (rs.next()) {
            val valores = cols.map { col =>
              val v = rs.getObject(col)
              if (v == null) "NULL"
              else v match {
                case s: String  => s"'${s.replace("'", "''")}'"
                case b: Boolean => if (b) "TRUE" else "FALSE"
                case _          => v.toString
              }
            }.mkString(", ")
            sb.append(s"INSERT INTO $tabla (${cols.mkString(", ")}) VALUES ($valores) ON CONFLICT DO NOTHING;\n")
          }
        } catch { case _: Exception => sb.append(s"-- TABLA $tabla no encontrada o vacia\n") }
      }
      sb.toString()
    } finally { conn.close() }
  }

  // Guarda el dump en BD (destino interno) y mantiene solo los ultimos 12 backups
  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE G — RESUMEN SEMANAL POR EMAIL (SQL puro, sin Gemini)
  // ─────────────────────────────────────────────────────────────────────────────
  def generarResumenSemanal(incluirAlertaCarga: Boolean = true, incluirDiario: Boolean = true): String = {
    val conn = getConnection()
    try {
      val hoy = LocalDate.now()

      // Partidos registrados en los ultimos 7 dias
      val rsM = conn.createStatement().executeQuery(
        "SELECT rival, goles_favor, goles_contra, nota FROM matches " +
        "WHERE status='PLAYED' AND fecha >= CURRENT_DATE - 7 ORDER BY fecha ASC")
      var partidosHtml = ""
      var numPartidos = 0
      while (rsM.next()) {
        numPartidos += 1
        partidosHtml += s"<li>${fixEncoding(rsM.getString("rival"))}: ${rsM.getInt("goles_favor")}-${rsM.getInt("goles_contra")} · nota ${rsM.getDouble("nota")}</li>"
      }
      if (numPartidos == 0) partidosHtml = "<li>Sin partidos esta semana</li>"

      // Entrenamientos registrados en los ultimos 7 dias, agrupados por tipo
      val rsT = conn.createStatement().executeQuery(
        "SELECT tipo, COUNT(*) as n FROM trainings WHERE fecha >= CURRENT_DATE - 7 GROUP BY tipo ORDER BY n DESC")
      var entrenosHtml = ""
      var numEntrenos = 0
      while (rsT.next()) {
        val n = rsT.getInt("n")
        numEntrenos += n
        entrenosHtml += s"<li>${fixEncoding(Option(rsT.getString("tipo")).getOrElse(""))}: $n sesión(es)</li>"
      }
      if (numEntrenos == 0) entrenosHtml = "<li>Sin entrenamientos esta semana</li>"

      // Dias con registro de sueno completado de los ultimos 7
      val rsW = conn.createStatement().executeQuery(
        "SELECT COUNT(DISTINCT fecha) as c FROM wellness WHERE fecha >= CURRENT_DATE - 7")
      val diasSueno = if (rsW.next()) rsW.getInt("c") else 0

      // Indice de Forma medio de la semana
      val rsForma = conn.createStatement().executeQuery(
        "SELECT AVG(indice_forma) as m FROM forma_diaria WHERE fecha >= CURRENT_DATE - 7")
      val formaMedia = if (rsForma.next()) rsForma.getDouble("m") else 0.0

      // Micro-objetivo de la semana
      val rsObj = conn.createStatement().executeQuery(
        "SELECT objetivo_semana, completado FROM micro_objetivos WHERE semana_inicio <= CURRENT_DATE " +
        "ORDER BY semana_inicio DESC LIMIT 1")
      val objetivoHtml = if (rsObj.next()) {
        val obj = fixEncoding(Option(rsObj.getString("objetivo_semana")).getOrElse(""))
        val comp = rsObj.getBoolean("completado")
        s"$obj — ${if (comp) "✅ completado" else "⏳ pendiente"}"
      } else "Sin micro-objetivo definido esta semana"

      // Alerta: mas de 10 dias sin registrar un partido jugado
      val rsUltimo = conn.createStatement().executeQuery("SELECT MAX(fecha) as f FROM matches WHERE status='PLAYED'")
      val alertaHtml = if (rsUltimo.next() && rsUltimo.getDate("f") != null) {
        val dias = java.time.temporal.ChronoUnit.DAYS.between(rsUltimo.getDate("f").toLocalDate, hoy)
        if (dias > 10) s"""<p style="color:#dc3545;"><strong>⚠️ Alerta:</strong> hace $dias días que no se registra un partido jugado.</p>""" else ""
      } else ""

      // BLOQUE G4: recordatorios trimestrales (>90 dias desde el ultimo registro)
      def diasDesdeUltimo(tabla: String): Option[Long] = {
        val rs = conn.createStatement().executeQuery(s"SELECT MAX(fecha) as f FROM $tabla")
        if (rs.next() && rs.getDate("f") != null) Some(java.time.temporal.ChronoUnit.DAYS.between(rs.getDate("f").toLocalDate, hoy)) else None
      }
      val pendientesTrimestre = scala.collection.mutable.ListBuffer[String]()
      diasDesdeUltimo("physical_tests").filter(_ > 90).foreach(d => pendientesTrimestre += s"💪 Test físico pendiente (último hace $d días)")
      diasDesdeUltimo("cognitivo_tests").filter(_ > 90).foreach(d => pendientesTrimestre += s"🧠 Test cognitivo pendiente (último hace $d días)")
      diasDesdeUltimo("psych_records").filter(_ > 90).foreach(d => pendientesTrimestre += s"🧠 Registro psicológico pendiente (último hace $d días)")
      diasDesdeUltimo("physical_growth").filter(_ > 90).foreach(d => pendientesTrimestre += s"📏 Registro de crecimiento pendiente (último hace $d días)")

      // MODULO LA VOZ DEL PORTERO: recordatorio si han pasado mas de 35 dias desde el ultimo registro
      if (debeRecordarVozPortero()) pendientesTrimestre += "🎤 La Voz del Portero de este mes pendiente."

      // BLOQUE A — recordatorio de revision mensual del IDP (solo si hay temporada IDP activa)
      val rsIdpRev = conn.createStatement().executeQuery(s"""
        SELECT
          ${DateUtils.daysFromTodaySQL("MAX(r.fecha)")} as dias
        FROM idp_revisiones r
        JOIN idp_temporadas t ON t.id = r.temporada_id
        WHERE t.estado = 'ACTIVA'
      """)
      if (rsIdpRev.next()) {
        val diasSinRevision = rsIdpRev.getInt("dias")
        if (!rsIdpRev.wasNull()) {
          if (diasSinRevision > 35)
            pendientesTrimestre += s"🗺️ Revisión mensual del IDP pendiente (última hace $diasSinRevision días) — ve a IDP para registrarla."
        } else {
          val rsIdpActiva = conn.createStatement().executeQuery(
            "SELECT COUNT(*) as n FROM idp_temporadas WHERE estado = 'ACTIVA'")
          if (rsIdpActiva.next() && rsIdpActiva.getInt("n") > 0)
            pendientesTrimestre += "🗺️ Aún no has hecho ninguna revisión mensual del IDP — ve a IDP para registrar la primera."
        }
      }

      val trimestralHtml = if (pendientesTrimestre.isEmpty) "" else
        s"<h3>📅 PENDIENTE TRIMESTRAL</h3><ul>${pendientesTrimestre.map(p => s"<li>$p</li>").mkString}</ul>"

      // BLOQUE C: aviso si la semana que entra es de carga escolar alta
      val cargaEscolarHtml = if (haySemanaEscolarCargada(7))
        """<p style="color:#fd7e14;"><strong>📚 Semana de carga escolar alta</strong> — reduce expectativas de rendimiento deportivo.</p>"""
      else ""

      // Diario narrativo: el primer lunes de mes, aviso con la primera frase del relato del mes anterior
      val diarioHtml = if (!incluirDiario) "" else asegurarDiarioMesAnterior().map { case (mes, contenido) =>
        val primeraFrase = contenido.split("(?<=[.!?])\\s+").headOption.getOrElse(contenido).take(220)
        s"""<p>📖 <strong>El diario narrativo de ${escHtml(mesLabel(mes))} está listo</strong> — ${escHtml(primeraFrase)}...</p>"""
      }.getOrElse("")

      // Protocolo de recuperacion en el email si el ACWR proyectado al sabado supera el umbral de riesgo
      val protocoloHtml = {
        val dow = hoy.getDayOfWeek.getValue
        val proyectado = if (dow <= 5) acwrProyectadoHasta(6 - dow) else None
        if (!proyectado.exists(_ > umbralesACWR().riesgo)) ""
        else generarProtocoloRecuperacion() match {
          case Right(texto) =>
            s"""<h3>🔄 PROTOCOLO DE RECUPERACIÓN ESTA SEMANA</h3><p style="white-space:pre-wrap;">${escHtml(texto)}</p>"""
          case Left(_) => ""
        }
      }

      // BLOQUE G: aviso si la calidad de datos bajo respecto a la semana anterior
      val calidadDatosHtml = dataQualityCambioSemanal().map(m => s"""<p style="color:#ca8a04;"><strong>${escHtml(m)}</strong></p>""").getOrElse("")

      // BLOQUE R: ACWR proyectado al sabado si se hacen todas las sesiones previstas
      val alertaCargaHtml = if (!incluirAlertaCarga) "" else
        predecirSobrecargaSemana().map(a => s"""<p style="color:#dc3545;"><strong>${escHtml(a)}</strong></p>""").getOrElse("")

      s"""
      <html><body style="font-family:sans-serif; color:#222;">
        <h2>Guardian Elite — Resumen semana del $hoy</h2>
        $diarioHtml
        $alertaHtml
        $alertaCargaHtml
        $protocoloHtml
        $calidadDatosHtml
        $cargaEscolarHtml
        <h3>⚽ Partidos ($numPartidos)</h3>
        <ul>$partidosHtml</ul>
        <h3>🏃 Entrenamientos ($numEntrenos)</h3>
        <ul>$entrenosHtml</ul>
        <h3>😴 Sueño registrado</h3>
        <p>$diasSueno de 7 días</p>
        <h3>📊 Índice de Forma medio</h3>
        <p>${"%.1f".format(formaMedia)}</p>
        <h3>🎯 Micro-objetivo de la semana</h3>
        <p>$objetivoHtml</p>
        $trimestralHtml
      </body></html>
      """
    } finally { conn.close() }
  }

  def guardarBackupEnBD(sql: String, fecha: String): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement(
        "INSERT INTO backups_log (fecha, tamano_kb, sql_dump, destinos) VALUES (?::date, ?, ?, ?)")
      ps.setString(1, fecha)
      ps.setInt(2, sql.getBytes("UTF-8").length / 1024)
      ps.setString(3, sql)
      ps.setString(4, "bd_interna")
      ps.executeUpdate()
      // Mantener solo los ultimos 12 (uno por semana ~ 3 meses)
      conn.createStatement().executeUpdate(
        "DELETE FROM backups_log WHERE id NOT IN (SELECT id FROM backups_log ORDER BY created_at DESC LIMIT 12)")
    } finally { conn.close() }
  }

  // Lista para el panel — sin traer el dump completo (puede ser pesado)
  def getBackupsLog(): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT id, fecha, tamano_kb, destinos, created_at FROM backups_log ORDER BY created_at DESC")
      var list = List[Map[String, Any]]()
      while (rs.next()) {
        list = list :+ Map[String, Any](
          "id" -> rs.getInt("id"),
          "fecha" -> rs.getDate("fecha").toString,
          "tamanoKb" -> rs.getInt("tamano_kb"),
          "destinos" -> Option(rs.getString("destinos")).getOrElse(""),
          "createdAt" -> Option(rs.getTimestamp("created_at")).map(_.toString).getOrElse("")
        )
      }
      list
    } finally { conn.close() }
  }

  // Recupera el dump completo de un backup concreto — usado para la descarga
  def getBackupSqlById(id: Int): Option[(String, String)] = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("SELECT fecha, sql_dump FROM backups_log WHERE id = ?")
      ps.setInt(1, id)
      val rs = ps.executeQuery()
      if (rs.next()) Some((rs.getDate("fecha").toString, rs.getString("sql_dump"))) else None
    } finally { conn.close() }
  }

  // ── MODULO 7: INFORME DE CAPTACION EXPORTABLE ───────────────────────────
  def getScoutingReportNarrative(edad: Int, notaMedia: Double, pctCS: Int, winRate: Int, acwr: Double, pj: Int): String = {
    val prompt = s"""Eres un ojeador profesional de fútbol base español redactando un informe de captación.
Portero de $edad años. Estadísticas: nota media ${f"$notaMedia%.1f"}/10, porterías a cero $pctCS%, win rate $winRate%, ACWR actual ${f"$acwr%.2f"}, $pj partidos registrados.
Entrena con su equipo (colectivo), asiste semanalmente a una academia específica de porteros, y complementa con judo como trabajo físico y de caídas.
Escribe un párrafo de 5-6 líneas en tercera persona, con el tono profesional de un informe de ojeador, que mencione explícitamente que combina entrenamiento colectivo + academia específica semanal + judo como complemento físico. Responde en texto plano, sin markdown."""
    AIProvider.ask(prompt)
  }

  // ACWR fisico real basado en Footbar: carga GPS objetiva (distancia x % actividad)
  // en lugar de la estimacion por minutos jugados. Devuelve 0.0 si no hay datos
  // suficientes en footbar_sessions durante los ultimos 28 dias.
  def getFootbarACWR(): Double = {
    val conn = getConnection()
    try {
      val rsAcute = conn.createStatement().executeQuery("""
        SELECT COALESCE(SUM(f.distancia_km * (f.pct_actividad / 100.0)), 0) AS carga
        FROM matches m JOIN footbar_sessions f ON f.match_id = m.id
        WHERE m.status='PLAYED' AND m.fecha >= CURRENT_DATE - 7
      """)
      val acuteSum = if (rsAcute.next()) rsAcute.getDouble("carga") else 0.0

      val rsChronic = conn.createStatement().executeQuery("""
        SELECT COALESCE(SUM(f.distancia_km * (f.pct_actividad / 100.0)), 0) AS carga
        FROM matches m JOIN footbar_sessions f ON f.match_id = m.id
        WHERE m.status='PLAYED' AND m.fecha >= CURRENT_DATE - 28
      """)
      val chronicSum = if (rsChronic.next()) rsChronic.getDouble("carga") else 0.0
      val chronicAvg = chronicSum / 4.0  // 28 dias = 4 semanas, equivalente semanal

      if (chronicAvg > 0) acuteSum / chronicAvg else 0.0
    } finally { conn.close() }
  }

  def playScheduledMatch(
                          id: Int, gf: Int, gc: Int, min: Int, nota: Double, paradas: Int,
                          notas: String, video: String, reaccion: String, clima: String, estadio: String,
                          zonaGoles: String, zonaTiros: String, zonaParadas: String,
                          p1v1: Int, pAir: Int, pPie: Int, pcTot: Int, pcOk: Int, plTot: Int, plOk: Int,
                          mapaCampo: String, // <--- NUEVO PARAMETRO
                          distanciaKm: Double = 0.0, // <--- FOOTBAR
                          comportamientoPresion: String = "", nutricionPrepartido: String = "",
                          cornersDominados: Int = 0, cornersCedidos: Int = 0, faltasAreaDominadas: Int = 0
                        ): Unit = {
    val conn = getConnection()
    try {
      val c = getLatestCardData()
      val n = StatsCalculator.calculateGrowth(c, min, gc, nota, paradas, pcTot, pcOk, plTot, plOk, distanciaKm)
      updateStats(n)

      val ps = conn.prepareStatement("""
      UPDATE matches SET
        status='PLAYED', goles_favor=?, goles_contra=?, minutos=?, nota=?, paradas=?,
        notas_partido=?, video_url=?, reaccion_goles=?, clima=?, estadio=?,
        zona_goles=?, zona_tiros=?, zona_paradas=?, paradas_1v1=?, paradas_aereas=?,
        acciones_pie=?, pc_t=?, pc_ok=?, pl_t=?, pl_ok=?, mapa_campo=?,
        corners_dominados=?, corners_cedidos=?, faltas_area_dominadas=?
      WHERE id=?
    """)

      ps.setInt(1, gf); ps.setInt(2, gc); ps.setInt(3, min); ps.setDouble(4, nota)
      ps.setInt(5, paradas); ps.setString(6, fixEncoding(notas)); ps.setString(7, video)
      ps.setString(8, fixEncoding(reaccion)); ps.setString(9, clima); ps.setString(10, fixEncoding(estadio))
      ps.setString(11, zonaGoles); ps.setString(12, zonaTiros); ps.setString(13, zonaParadas)
      ps.setInt(14, p1v1); ps.setInt(15, pAir); ps.setInt(16, pPie)
      ps.setInt(17, pcTot); ps.setInt(18, pcOk); ps.setInt(19, plTot); ps.setInt(20, plOk)
      ps.setString(21, mapaCampo) // <--- NUEVO
      ps.setInt(22, cornersDominados); ps.setInt(23, cornersCedidos); ps.setInt(24, faltasAreaDominadas)
      ps.setInt(25, id)

      ps.executeUpdate()

      if (comportamientoPresion.nonEmpty && comportamientoPresion != "NA") {
        val ps2 = conn.prepareStatement("UPDATE matches SET comportamiento_presion = ? WHERE id = ?")
        ps2.setString(1, comportamientoPresion); ps2.setInt(2, id)
        ps2.executeUpdate()
      }
      if (nutricionPrepartido.nonEmpty) {
        val ps3 = conn.prepareStatement("UPDATE matches SET nutricion_prepartido = ? WHERE id = ?")
        ps3.setString(1, nutricionPrepartido); ps3.setInt(2, id)
        ps3.executeUpdate()
      }
    } finally {
      conn.close()
    }
    linkFormaDiariaAMatch(id)
  }

  // ── BLOQUE A4: vincula el partido guardado con el registro de forma del dia (o el anterior) ─
  private def linkFormaDiariaAMatch(matchId: Int): Unit = {
    val conn = getConnection()
    try {
      val rs = conn.prepareStatement("SELECT fecha, nota FROM matches WHERE id = ?")
      rs.setInt(1, matchId)
      val r = rs.executeQuery()
      if (r.next()) {
        val fechaPartido = r.getDate("fecha")
        val nota = r.getDouble("nota")
        if (fechaPartido != null && nota > 0) {
          val fechaLocal = fechaPartido.toLocalDate
          val upd = conn.prepareStatement("UPDATE forma_diaria SET nota_partido = ?, match_id = ? WHERE fecha IN (?, ?)")
          upd.setDouble(1, nota); upd.setInt(2, matchId)
          upd.setDate(3, Date.valueOf(fechaLocal)); upd.setDate(4, Date.valueOf(fechaLocal.minusDays(1)))
          upd.executeUpdate()
        }
      }
    } finally { conn.close() }
  }

  // --- LECTURA DE PARTIDOS EXTENDIDA ---
  def getMatchesList(seasonId: Int = 0): List[MatchLog] = { var l=List[MatchLog](); val conn=getConnection(); try{ val rs=conn.createStatement().executeQuery(s"SELECT * FROM matches WHERE status='PLAYED' ${seasonFilter(seasonId)} ORDER BY fecha DESC"); while(rs.next()){ l=l:+MatchLog(rs.getInt("id"), rs.getString("rival"), s"${rs.getInt("goles_favor")}-${rs.getInt("goles_contra")}", rs.getInt("minutos"), rs.getDouble("nota"), rs.getDate("fecha").toString, Option(rs.getString("clima")).getOrElse(""), Option(rs.getString("estadio")).getOrElse(""), Option(rs.getString("notas_partido")).getOrElse(""), Option(rs.getString("video_url")).getOrElse(""), Option(rs.getString("reaccion_goles")).getOrElse(""), rs.getString("status"), Option(rs.getString("tipo_partido")).getOrElse("LIGA"), rs.getInt("pc_t"), rs.getInt("pc_ok"), rs.getInt("pl_t"), rs.getInt("pl_ok"), Option(rs.getString("analisis_voz")).getOrElse(""), Option(rs.getString("torneo_nombre")).getOrElse(""), Option(rs.getString("fase")).getOrElse(""), rs.getInt("paradas"), rs.getInt("paradas_1v1"), rs.getInt("paradas_aereas"), rs.getInt("acciones_pie"), Option(rs.getString("zona_tiros")).getOrElse(""), Option(rs.getString("zona_goles")).getOrElse(""), { val cpiV = rs.getDouble("cpi"); if (rs.wasNull()) None else Some(cpiV) }) } } finally {conn.close()}; l }
  def getUpcomingMatches(): List[MatchLog] = { var l=List[MatchLog](); val conn=getConnection(); try{ val rs=conn.createStatement().executeQuery("SELECT * FROM matches WHERE status='SCHEDULED' ORDER BY fecha ASC"); while(rs.next()){ l=l:+MatchLog(rs.getInt("id"), rs.getString("rival"), "-", 0, 0, rs.getDate("fecha").toString, "", Option(rs.getString("estadio")).getOrElse(""), "", "", "", rs.getString("status"), Option(rs.getString("tipo_partido")).getOrElse("LIGA"),0,0,0,0, "", Option(rs.getString("torneo_nombre")).getOrElse(""), Option(rs.getString("fase")).getOrElse(""), 0,0,0,0,"","") } } finally {conn.close()}; l }
  def getMatchById(id: Int): Option[MatchLog] = { var m:Option[MatchLog]=None; val conn=getConnection(); try { val s=conn.prepareStatement("SELECT * FROM matches WHERE id = ?"); s.setInt(1,id); val rs=s.executeQuery(); if(rs.next()){ m=Some(MatchLog(rs.getInt("id"), rs.getString("rival"), s"${rs.getInt("goles_favor")}-${rs.getInt("goles_contra")}", rs.getInt("minutos"), rs.getDouble("nota"), rs.getDate("fecha").toString, Option(rs.getString("clima")).getOrElse("Sol"), Option(rs.getString("estadio")).getOrElse(""), Option(rs.getString("notas_partido")).getOrElse(""), Option(rs.getString("video_url")).getOrElse(""), Option(rs.getString("reaccion_goles")).getOrElse(""), rs.getString("status"), Option(rs.getString("tipo_partido")).getOrElse("LIGA"), rs.getInt("pc_t"), rs.getInt("pc_ok"), rs.getInt("pl_t"), rs.getInt("pl_ok"), Option(rs.getString("analisis_voz")).getOrElse(""), Option(rs.getString("torneo_nombre")).getOrElse(""), Option(rs.getString("fase")).getOrElse(""), rs.getInt("paradas"), rs.getInt("paradas_1v1"), rs.getInt("paradas_aereas"), rs.getInt("acciones_pie"), Option(rs.getString("zona_tiros")).getOrElse(""), Option(rs.getString("zona_goles")).getOrElse(""))) } } finally { conn.close() }; m }
  def getRivalScouting(rivalBusqueda: String): (List[MatchLog], Map[String, Int]) = { var matches = List[MatchLog](); var stats = scala.collection.mutable.Map("pj"->0, "gf"->0, "gc"->0, "ganados"->0, "empatados"->0, "perdidos"->0); val conn = getConnection(); try { val query = s"SELECT * FROM matches WHERE LOWER(rival) LIKE LOWER(?) AND status='PLAYED' ORDER BY fecha DESC"; val stmt = conn.prepareStatement(query); stmt.setString(1, s"%$rivalBusqueda%"); val rs = stmt.executeQuery(); while(rs.next()) { val (gf, gc) = (rs.getInt("goles_favor"), rs.getInt("goles_contra")); matches = matches :+ MatchLog(rs.getInt("id"), rs.getString("rival"), s"$gf-$gc", rs.getInt("minutos"), rs.getDouble("nota"), rs.getString("fecha"), Option(rs.getString("clima")).getOrElse(""), Option(rs.getString("estadio")).getOrElse(""), Option(rs.getString("notas_partido")).getOrElse(""), Option(rs.getString("video_url")).getOrElse(""), Option(rs.getString("reaccion_goles")).getOrElse(""), rs.getString("status"), Option(rs.getString("tipo_partido")).getOrElse("LIGA"), rs.getInt("pc_t"), rs.getInt("pc_ok"), rs.getInt("pl_t"), rs.getInt("pl_ok"), Option(rs.getString("analisis_voz")).getOrElse(""), Option(rs.getString("torneo_nombre")).getOrElse(""), Option(rs.getString("fase")).getOrElse(""), rs.getInt("paradas"), rs.getInt("paradas_1v1"), rs.getInt("paradas_aereas"), rs.getInt("acciones_pie"), Option(rs.getString("zona_tiros")).getOrElse(""), Option(rs.getString("zona_goles")).getOrElse("")); stats("pj") += 1; stats("gf") += gf; stats("gc") += gc; if(gf > gc) stats("ganados") += 1 else if(gf == gc) stats("empatados") += 1 else stats("perdidos") += 1 } } finally { conn.close() }; (matches, stats.toMap) }

  // --- FUNCIONES EXTRA ---
  def createTournament(nombre: String, estructura: String): String = { val conn=getConnection(); var count=0; try{ val rsId=conn.createStatement().executeQuery("SELECT MAX(id) as id FROM seasons"); if(rsId.next()){ val sId=rsId.getInt("id"); val lines=estructura.split("\n").map(_.trim).filter(_.nonEmpty); val ps=conn.prepareStatement("INSERT INTO matches (season_id, fecha, rival, tipo_partido, status, goles_favor, goles_contra, minutos, nota, paradas, clima, estadio, torneo_nombre, fase) VALUES (?, ?, ?, 'TORNEO', 'SCHEDULED', 0, 0, 0, 0, 0, 'Sol', 'Sede Torneo', ?, ?)"); lines.foreach { l => val p=l.split("\\|").map(_.trim); if(p.length>=2){ ps.setInt(1, sId); ps.setDate(2, if(p.length>2) try Date.valueOf(p(2)) catch {case _:Exception=>Date.valueOf(LocalDate.now())} else Date.valueOf(LocalDate.now())); ps.setString(3, fixEncoding(p(1))); ps.setString(4, fixEncoding(nombre)); ps.setString(5, fixEncoding(p(0))); ps.executeUpdate(); count += 1 } } } else return "Error: Crea una temporada primero." } catch { case e: Exception => return s"Error: ${e.getMessage}" } finally { conn.close() }; s"Torneo '$nombre' creado ($count partidos)." }
  def syncRFFMCalendar(): String = {
    var logs = new StringBuilder(); var count = 0
    val conn = getConnection()
    try {
      val rsCfg = conn.createStatement().executeQuery("SELECT id, rffm_url, rffm_team_name FROM seasons ORDER BY id DESC LIMIT 1")
      if (!rsCfg.next()) return "Error: Sin temporada."
      val (sid, url, myTeam) = (rsCfg.getInt("id"), Option(rsCfg.getString("rffm_url")).getOrElse(""), Option(rsCfg.getString("rffm_team_name")).getOrElse("").toUpperCase)
      if (url.isEmpty || myTeam.isEmpty) return "Error Config."

      // BLOQUE H2: encoding explicito — si Jsoup no detecta UTF-8, re-parsea forzando la conversion
      val doc = Jsoup.connect(url).userAgent("Mozilla/5.0").timeout(10000).get()
      val docFinal = if (doc.charset().name() != "UTF-8")
        Jsoup.parse(new String(doc.html().getBytes("ISO-8859-1"), "UTF-8"))
      else doc

      val ps = conn.prepareStatement("INSERT INTO matches (season_id, fecha, rival, tipo_partido, status, goles_favor, goles_contra, minutos, nota, paradas, clima, estadio, torneo_nombre, fase) VALUES (?, ?, ?, 'LIGA', 'SCHEDULED', 0, 0, 0, 0, 0, 'Sol', ?, '', 'Regular')")
      for (row <- docFinal.select("table tbody tr").asScala) {
        val cols = row.select("td")
        if (cols.size() >= 4) {
          val (loc, vis) = (cols.get(0).text().toUpperCase.trim, cols.get(2).text().toUpperCase.trim)
          if (loc.contains(myTeam) || vis.contains(myTeam)) {
            val rival = if (loc.contains(myTeam)) vis else loc
            val campo = if (cols.get(3).text().length > 50) cols.get(3).text().take(50) else cols.get(3).text()
            if (conn.createStatement().executeQuery(s"SELECT count(*) FROM matches WHERE season_id=$sid AND rival='${fixEncoding(rival)}'").next()) {
              ps.setInt(1, sid); ps.setDate(2, Date.valueOf(LocalDate.now().plusDays(7)))
              ps.setString(3, fixEncoding(rival)); ps.setString(4, fixEncoding(campo))
              ps.executeUpdate(); count += 1; logs.append(s"+ $rival\n")
            }
          }
        }
      }
    } catch { case e: Exception => logs.append(s"Err: ${e.getMessage}") } finally { conn.close() }
    logs.toString()
  }
  def updateMatch(id: Int, rival: String, gf: Int, gc: Int, min: Int, nota: Double, clima: String, estadio: String, temp: Int, notas: String, video: String, reaccion: String, fechaStr: String): Unit = { val conn=getConnection(); try { val s=conn.prepareStatement("UPDATE matches SET rival=?, goles_favor=?, goles_contra=?, minutos=?, nota=?, clima=?, estadio=?, temperatura=?, notas_partido=?, video_url=?, reaccion_goles=?, fecha=? WHERE id=?"); s.setString(1,fixEncoding(rival)); s.setInt(2,gf); s.setInt(3,gc); s.setInt(4,min); s.setDouble(5,nota); s.setString(6,clima); s.setString(7,fixEncoding(estadio)); s.setInt(8,temp); s.setString(9,fixEncoding(notas)); s.setString(10,video); s.setString(11,fixEncoding(reaccion)); s.setDate(12,Date.valueOf(fechaStr)); s.setInt(13,id); s.executeUpdate() } finally { conn.close() }; linkFormaDiariaAMatch(id) }
  def updateMatchExtra(id: Int, tipo: String, esLocal: String): Unit = { val conn=getConnection(); try { val esLocalVal: java.lang.Boolean = esLocal match { case "true" => true; case "false" => false; case _ => null }; val ps = conn.prepareStatement("UPDATE matches SET tipo_partido=? WHERE id=?"); ps.setString(1, if (tipo.nonEmpty) tipo else "LIGA"); ps.setInt(2, id); ps.executeUpdate(); if (esLocalVal != null) { val ps2 = conn.prepareStatement("UPDATE matches SET es_local=? WHERE id=?"); ps2.setBoolean(1, esLocalVal); ps2.setInt(2, id); ps2.executeUpdate() } } finally { conn.close() } }
  def deleteMatch(id: Int): Unit = { val conn=getConnection(); try { conn.createStatement().executeUpdate(s"DELETE FROM matches WHERE id=$id") } finally { conn.close() } }
  def updateRFFMSettings(url: String, teamName: String): Unit = { val conn=getConnection(); try{ val ps=conn.prepareStatement("UPDATE seasons SET rffm_url=?, rffm_team_name=? WHERE id=(SELECT MAX(id) FROM seasons)"); ps.setString(1,url); ps.setString(2,teamName); ps.executeUpdate() } finally { conn.close() } }
  def updateSeasonSettings(f: String, c: String, n: String, fecha: String): String = { val conn=getConnection(); try { val s=conn.prepareStatement("UPDATE seasons SET foto_jugador_url=COALESCE(NULLIF(?,''), foto_jugador_url), club_escudo_url=COALESCE(NULLIF(?,''), club_escudo_url), nombre_club=COALESCE(NULLIF(?,''), nombre_club), fecha_nacimiento=? WHERE id=(SELECT MAX(id) FROM seasons)"); s.setString(1,f); s.setString(2,c); s.setString(3,fixEncoding(n)); s.setDate(4, Date.valueOf(fecha)); s.executeUpdate(); "DATOS ACTUALIZADOS" } finally { conn.close() } }
  def getLatestCardData(): PlayerCardData = { var conn: Connection=null; try { conn=getConnection(); val rs=conn.createStatement().executeQuery("SELECT * FROM seasons ORDER BY id DESC LIMIT 1"); if(rs.next()){ val fecha=Option(rs.getDate("fecha_nacimiento")).map(_.toString).getOrElse("2020-06-19"); PlayerCardData("HECTOR", rs.getDouble("media").toInt, "GK", Option(rs.getString("foto_jugador_url")).getOrElse(""), Option(rs.getString("club_escudo_url")).getOrElse(""), "", Option(rs.getString("nombre_club")).getOrElse(""), rs.getDouble("stat_div").toInt, rs.getDouble("stat_han").toInt, rs.getDouble("stat_kic").toInt, rs.getDouble("stat_ref").toInt, rs.getDouble("stat_spd").toInt, rs.getDouble("stat_pos").toInt, rs.getDouble("stat_div"), rs.getDouble("stat_han"), rs.getDouble("stat_kic"), rs.getDouble("stat_ref"), rs.getDouble("stat_spd"), rs.getDouble("stat_pos"), fecha, Option(rs.getString("rffm_url")).getOrElse(""), Option(rs.getString("rffm_team_name")).getOrElse(""), Option(rs.getString("categoria")).getOrElse("Prebenjamín")) } else { PlayerCardData("HECTOR", 59, "GK", "", "", "", "", 80, 60, 55, 60, 62, 58, 80, 60, 55, 60, 62, 58, "2020-06-19", "", "", "Prebenjamín") } } finally { if(conn!=null) conn.close() } }
  def getDeepAnalysis(): String = {
    var conn:Connection=null;
    try {
      conn=getConnection(); val sb=new StringBuilder(); val card=getLatestCardData(); val edad=calcularEdadExacta(card.fechaNacimiento);
      sb.append(s"Analista Elite ($edad anos). Tendencias:\n");
      val rs=conn.createStatement().executeQuery(s"""
        SELECT m.fecha, m.rival, m.nota,
               COALESCE(f.distancia_km, 0)      AS dist_km,
               COALESCE(f.sprint_max_kmh, 0)    AS sprint_max,
               COALESCE(f.pases, 0)             AS pases
        FROM matches m
        LEFT JOIN footbar_sessions f ON f.match_id = m.id
        WHERE m.status='PLAYED' ${seasonFilterActual("m")}
        ORDER BY m.fecha ASC
      """);
      var c=0; while(rs.next()){ c+=1; sb.append(s"${rs.getString("fecha")}|${rs.getString("rival")}|${rs.getDouble("nota")}|${rs.getDouble("dist_km")}|${rs.getDouble("sprint_max")}|${rs.getInt("pases")}\n") };
      if(c<2) return "Pocos datos.";
      val fechaHoy = java.time.LocalDate.now().toString
      val anio = java.time.LocalDate.now().getYear
      val temporadaActual = s"$anio-${anio + 1}"

      val rsBascula = conn.createStatement().executeQuery(
        "SELECT kg_musculo, kg_masa_osea FROM physical_growth WHERE kg_musculo IS NOT NULL ORDER BY fecha DESC LIMIT 1")
      val basculaLine = if (rsBascula.next())
        s"\nDatos de composición corporal (báscula inteligente): músculo ${rsBascula.getDouble("kg_musculo")}kg, masa ósea ${rsBascula.getDouble("kg_masa_osea")}kg.\n"
      else ""

      val rubricaLine = getRubricaMediasTemporada() match {
        case Some(r) =>
          s"\nMedias de la rúbrica de valoración esta temporada (escala 1-5): posición ${"%.1f".format(r("posicion"))}, decisiones bajo presión ${"%.1f".format(r("decisiones"))}, juego con los pies ${"%.1f".format(r("pies"))}, comunicación ${"%.1f".format(r("comunicacion"))}, actitud ${"%.1f".format(r("actitud"))}.\n"
        case None => ""
      }

      // BLOQUE C: contexto avanzado del ultimo partido, cuando esta disponible
      val rsContexto = conn.createStatement().executeQuery("""
        SELECT calentamiento_min, calentamiento_tipo, superficie, factores_externos,
               velocidad_distribucion, economia_movimiento, calidad_decision_pct
        FROM matches WHERE status='PLAYED' ORDER BY fecha DESC LIMIT 1""")
      val contextoLine = if (rsContexto.next()) {
        val partes = scala.collection.mutable.ListBuffer[String]()
        val calMin = rsContexto.getInt("calentamiento_min"); if (!rsContexto.wasNull()) partes += s"calentamiento de $calMin min"
        Option(rsContexto.getString("calentamiento_tipo")).filter(_.nonEmpty).foreach(t => partes += s"tipo $t")
        Option(rsContexto.getString("superficie")).filter(_.nonEmpty).foreach(s => partes += s"césped $s")
        Option(rsContexto.getString("velocidad_distribucion")).filter(_.nonEmpty).foreach(v => partes += s"velocidad de distribución $v")
        val econ = rsContexto.getInt("economia_movimiento"); if (!rsContexto.wasNull()) partes += s"economía de movimiento $econ/5"
        val calidad = rsContexto.getInt("calidad_decision_pct"); if (!rsContexto.wasNull()) partes += s"calidad de decisión $calidad%"
        Option(rsContexto.getString("factores_externos")).filter(_.nonEmpty).foreach(f => partes += s"factor externo relevante: $f")
        if (partes.isEmpty) "" else s"\nContexto avanzado del último partido: ${partes.mkString(", ")}.\n"
      } else ""

      // BLOQUE A: deuda de sueno acumulada — solo si es relevante (MODERADA o superior)
      val deudaHoy = calcularDeudaSueno()
      val deudaNivelHoy = deudaHoy("nivel").asInstanceOf[String]
      val deudaLine =
        if (deudaNivelHoy == "MINIMA") ""
        else s"\nDeuda de sueño acumulada esta semana: ${"%.1f".format(deudaHoy("deudaHoras").asInstanceOf[Double])}h (nivel: $deudaNivelHoy). Media de ${"%.1f".format(deudaHoy("mediaDiaria").asInstanceOf[Double])}h/noche, óptimo 10h para su edad.\n"

      // BLOQUE C: carga cognitiva escolar (examenes/fin de trimestre) — SQL puro, sin Gemini
      val cargaEscolarLine = getPeriodoEscolarHoy() match {
        case Some("EXAMENES") | Some("TRIMESTRE_FIN") =>
          "\nEsta semana coincide con período de exámenes escolares — la bajada de energía o rendimiento puede tener origen escolar, no deportivo.\n"
        case _ => ""
      }

      // MODULO LA VOZ DEL PORTERO: tendencia de motivacion declarada por Hector, si hay registros recientes
      val vozPorteroLine = getVozPorteroTendenciaTexto().map(t => s"\n$t\n").getOrElse("")

      // BLOQUE N: desglose de habilidades por nivel de automatismo
      val automatismo = getAutomatismoBreakdown()
      val automatismoLine =
        if (automatismo.values.sum == 0) ""
        else s"\nHabilidades instintivas: ${automatismo("instintivas")} · Automáticas: ${automatismo("automaticas")} · Conscientes: ${automatismo("conscientes")}. Las habilidades conscientes son el área de trabajo actual — son las que fallan primero bajo presión.\n"

      // BLOQUE H: CPI medio de la temporada, cuando esta disponible
      val cpiLine = getCpiMedioTemporada() match {
        case Some(cpiMedio) => s"\nSu CPI medio de la temporada es ${"%.1f".format(cpiMedio)} — ajustado por dificultad real de cada partido (rival, condiciones físicas, clima, si jugó en casa o fuera). Da más peso a este dato que a la nota media simple.\n"
        case None => ""
      }

      // BLOQUE B: sesgo de valoracion por resultado del padre, como aviso metodologico
      val sesgoLine = {
        val sg = calcularSesgoPorResultado(getTemporadaActivaId())
        if (!sg("sesgo").asInstanceOf[Boolean]) ""
        else f"\nAVISO: El padre muestra sesgo de valoración por resultado (r=${sg("correlacion").asInstanceOf[Option[Double]].get}%.2f). Las notas pueden estar infladas en victorias y defladas en derrotas. Tenerlo en cuenta al interpretar la evolución de la nota.\n"
      }

      // Nutricion e hidratacion pre-partido, cuando hay datos (al menos 5 partidos)
      val nutricionLine = {
        val nu = getNutricionAnalysis(getTemporadaActivaId())
        val hid = nu("hidratacion").asInstanceOf[Map[String, (Double, Int)]]
        if (nu("partidos").asInstanceOf[Int] < 5 || hid.isEmpty) ""
        else "\nNutrición pre-partido — nota media por hidratación: " +
          hid.map { case (k, (nota, n)) => f"${etiquetasHidratacion.getOrElse(k, k).drop(2).trim} $nota%.1f ($n partidos)" }.mkString(", ") + ".\n"
      }

      // BLOQUE J: calibracion del padre como observador (solo si hay desviacion)
      val calibracionLine = getUltimaCalibracion().filter(_("desviacion").asInstanceOf[Double] != 0).map { c =>
        val d = c("desviacion").asInstanceOf[Double]
        val etiqueta = dimensionesRubrica.find(_._1 == c("dimension").toString).map(_._3).getOrElse(c("dimension").toString)
        f"\nNota metodológica: el padre tiende a puntuar ${etiqueta.toLowerCase} ${math.abs(d)}%.0f puntos por ${if (d < 0) "debajo" else "encima"} de la referencia según su calibración de ${c("fecha")}.\n"
      }.getOrElse("")

      // Cambio aqui: Llamamos a AIProvider.ask
      val prompt = s"""Eres un analista de rendimiento de porteros de élite. Fecha de hoy: $fechaHoy. Temporada en curso: $temporadaActual. Analiza ÚNICAMENTE los datos de esta temporada.

CONTEXTO FOOTBAR — PORTERO: Héctor es portero. Los porteros recorren estructuralmente mucha menos distancia que los jugadores de campo — 1-2km en un partido de Fútbol 7 es completamente normal y no indica baja implicación. Los sprints son pocos pero explosivos. El porcentaje de actividad es bajo por naturaleza del puesto. NO uses los datos de Footbar para evaluar el nivel de esfuerzo o la implicación de Héctor en el partido. Úsalos ÚNICAMENTE para: detectar si hubo más movimiento lateral de lo habitual (puede indicar mayor presión sobre su portería) y para cruzar con la recuperación física (distancias superiores a 2.5km para un portero son excepcionalmente exigentes y requieren más descanso).

Tienes los siguientes partidos de Hector (portero, ${edad} años), con formato fecha|rival|nota|distanciaKm|sprintMaxKmh|pases (los tres ultimos son datos del sensor Footbar; 0 si no se registraron para ese partido):

${sb.toString()}$basculaLine$rubricaLine$contextoLine$deudaLine$cargaEscolarLine$automatismoLine$vozPorteroLine$cpiLine$sesgoLine$calibracionLine$nutricionLine

Escribe un análisis narrativo en HTML limpio (sin markdown, sin bloques de código). Usa exactamente esta estructura:
<h4>ANÁLISIS</h4>
<p><strong>Tendencia general:</strong> [un párrafo describiendo la evolución de las notas a lo largo del tiempo, si va subiendo, bajando, o irregular]</p>
<p><strong>Mejor racha:</strong> [describe el período de mejor rendimiento y contra qué rivales]</p>
<p><strong>Punto de atención:</strong> [describe el momento más bajo y posibles causas]</p>
<p><strong>Conclusión:</strong> [una frase motivadora y concreta sobre qué trabajar para la próxima semana]</p>

No reproduzcas la tabla de datos. Escribe siempre en párrafos. Habla en segunda persona dirigiéndote a Hector directamente. Si hay datos de distancia y sprint, analiza si hay correlación entre carga física y rendimiento. Si distancia > 3km con nota baja, o sprint alto con nota baja, menciónalo como señal de fatiga. Habla siempre en presente o futuro próximo. No menciones fechas del año anterior. La temporada actual es $temporadaActual."""
      val analisis = AIProvider.ask(prompt).replace("```html","").replace("```","").trim

      // BLOQUE B: conducta del padre — NUNCA se envia a Gemini (dato privado sobre el padre, no
      // sobre Hector); se anade de forma deterministica al final si la media reciente es baja.
      val conductaPadreLine = {
        val cp = getConductaPadreAnalysis()
        if (cp("suficiente").asInstanceOf[Boolean] && cp("mediaConducta").asInstanceOf[Double] < 3.0)
          "<p><strong>Nota privada:</strong> La conducta del padre en la banda ha sido valorada como intervencionista en los últimos partidos. Esto puede estar influyendo en el estado emocional de Héctor.</p>"
        else ""
      }
      analisis + conductaPadreLine
    } catch {
      case e:Exception =>
        e.printStackTrace() // Esto hara que el error aparezca en el log de Render/Consola
        "Error: " + e.getMessage
    }
  }
  def getChartData(): String = { var l=List[String](); var d=List[Double](); val conn=getConnection(); try{ val rs=conn.createStatement().executeQuery("SELECT rival, media_historica FROM matches WHERE status='PLAYED' ORDER BY fecha ASC LIMIT 15"); while(rs.next()){ l=l:+s"'${rs.getString("rival")}'"; d=d:+rs.getDouble("media_historica") } } finally {conn.close()}; s"""{ "labels": [${l.mkString(",")}], "data": [${d.mkString(",")}] }""" }
  def getAchievements(): List[Achievement] = { var l=List[Achievement](); val conn=getConnection(); try { val s=conn.createStatement(); val r1=s.executeQuery("SELECT COUNT(*) FROM matches WHERE goles_contra=0 AND status='PLAYED'"); if(r1.next()&&r1.getInt(1)>=5) l=l:+Achievement("(M)","El Muro",r1.getInt(1)/5,""); val r2=s.executeQuery("SELECT COUNT(*) FROM matches WHERE nota>=9 AND status='PLAYED'"); if(r2.next()&&r2.getInt(1)>0) l=l:+Achievement("(E)","MVP",r2.getInt(1),"") } finally { conn.close() }; l }
  def getSeasonObjectives(): List[Objective] = { var l=List[Objective](); val conn=getConnection(); try { val rsObj=conn.createStatement().executeQuery("SELECT id, tipo, objetivo, descripcion FROM objectives"); val objs=new scala.collection.mutable.ListBuffer[(Int,String,Int,String)](); while(rsObj.next()) objs+=((rsObj.getInt("id"),rsObj.getString("tipo"),rsObj.getInt("objetivo"),rsObj.getString("descripcion"))); val rsStats=conn.createStatement().executeQuery("SELECT COUNT(*) as pj, COUNT(CASE WHEN goles_contra=0 THEN 1 END) as cs, AVG(nota) as media FROM matches WHERE status='PLAYED'"); var (cs,pj,md)=(0,0,0.0); if(rsStats.next()){cs=rsStats.getInt("cs");pj=rsStats.getInt("pj");md=rsStats.getDouble("media")}; objs.foreach { case (id,t,m,d) => val act=t match { case "CleanSheets"=>cs.toDouble case "MediaNota"=>md case "PartidosJugados"=>pj.toDouble case _=>0.0 }; l=l:+Objective(id,t,act,m,d) } } finally { conn.close() }; l }
  def getGoalHeatmap(temporada: String = "", seasonId: Int = 0): Map[String, Int] = {
    val zones = Seq("TL","TC","TR","ML","MC","MR","BL","BC","BR")
    val counts = scala.collection.mutable.Map(zones.map(_ -> 0): _*)
    val conn = getConnection()
    try {
      val whereAnio = if (temporada.nonEmpty) s"AND fecha >= '$temporada-01-01' AND fecha <= '$temporada-12-31'" else ""
      val rs = conn.createStatement().executeQuery(
        s"SELECT zona_goles FROM matches WHERE status='PLAYED' AND zona_goles IS NOT NULL AND zona_goles != '' $whereAnio ${seasonFilter(seasonId)}"
      )
      while (rs.next()) {
        val zg = rs.getString("zona_goles")
        zg.split(",").filter(_.nonEmpty).foreach { z =>
          val key = z.trim.toUpperCase
          if (counts.contains(key)) counts(key) += 1
        }
      }
    } finally { conn.close() }
    counts.toMap
  }

  // BLOQUE P: mapa de calor de 6 zonas sobre el mismo recuento que getGoalHeatmap (9 zonas T/M/B x L/C/R).
  // La fila de media altura (M*) se suma a BAJO: por debajo de la mitad de la porteria.
  val zonasPorteria6: Seq[(String, String)] = Seq(
    "ALTO_IZQ" -> "Alto izquierda", "ALTO_CEN" -> "Alto centro", "ALTO_DER" -> "Alto derecha",
    "BAJO_IZQ" -> "Bajo izquierda", "BAJO_CEN" -> "Bajo centro", "BAJO_DER" -> "Bajo derecha")

  def getGoalHeatmap6Zonas(seasonId: Int = 0): Map[String, Any] = {
    val nueve = getGoalHeatmap(seasonId = seasonId)
    def zona6(codigo: String): String =
      (if (codigo.startsWith("T")) "ALTO" else "BAJO") + "_" + (codigo.last match { case 'L' => "IZQ"; case 'C' => "CEN"; case _ => "DER" })
    val zonas = zonasPorteria6.map { case (k, _) => k -> 0 }.toMap ++
      nueve.toSeq.groupBy { case (c, _) => zona6(c) }.map { case (z, l) => z -> l.map(_._2).sum }
    val total = zonas.values.sum
    val orden = zonasPorteria6.map(_._1).map(z => z -> zonas(z))
    Map("zonas" -> zonas, "total" -> total, "suficiente" -> (total >= 10),
      "zonaMax" -> orden.maxBy(_._2)._1, "zonaMin" -> orden.minBy(_._2)._1)
  }

  def getGoalHeatmapByRival(rival: String): Map[String, Int] = {
    val zones = Seq("TL","TC","TR","ML","MC","MR","BL","BC","BR")
    val counts = scala.collection.mutable.Map(zones.map(_ -> 0): _*)
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement(
        "SELECT zona_goles FROM matches WHERE status='PLAYED' AND LOWER(rival) LIKE LOWER(?) AND zona_goles IS NOT NULL AND zona_goles != ''"
      )
      ps.setString(1, s"%$rival%")
      val rs = ps.executeQuery()
      while (rs.next()) {
        rs.getString("zona_goles").split(",").filter(_.nonEmpty).foreach { z =>
          val key = z.trim.toUpperCase
          if (counts.contains(key)) counts(key) += 1
        }
      }
    } finally { conn.close() }
    counts.toMap
  }

  def getPenaltyDetailedStats(): (Map[String, (Int,Int)], List[(String,Int,Int)]) = {
    // (zona -> (total, parados)), List[(rival, total, goles)]
    val byZone  = scala.collection.mutable.Map[String,(Int,Int)]()
    val byRival = scala.collection.mutable.ListBuffer[(String,Int,Int)]()
    val conn = getConnection()
    try {
      val rs1 = conn.createStatement().executeQuery(
        "SELECT zona_tiro, COUNT(*) as total, SUM(CASE WHEN es_gol THEN 0 ELSE 1 END) as parados FROM penalties GROUP BY zona_tiro ORDER BY total DESC"
      )
      while (rs1.next()) byZone(rs1.getString("zona_tiro")) = (rs1.getInt("total"), rs1.getInt("parados"))
      val rs2 = conn.createStatement().executeQuery(
        "SELECT rival, COUNT(*) as total, SUM(CASE WHEN es_gol THEN 1 ELSE 0 END) as goles FROM penalties WHERE rival IS NOT NULL AND rival != '' GROUP BY rival ORDER BY total DESC LIMIT 10"
      )
      while (rs2.next()) byRival += ((rs2.getString("rival"), rs2.getInt("total"), rs2.getInt("goles")))
    } finally { conn.close() }
    (byZone.toMap, byRival.toList)
  }

  def getTournamentMatches(nombre: String): List[MatchLog] = {
    var l = List[MatchLog]()
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("SELECT * FROM matches WHERE LOWER(torneo_nombre)=LOWER(?) ORDER BY id ASC")
      ps.setString(1, fixEncoding(nombre))
      val rs = ps.executeQuery()
      while (rs.next()) l = l :+ MatchLog(
        rs.getInt("id"), rs.getString("rival"),
        s"${rs.getInt("goles_favor")}-${rs.getInt("goles_contra")}",
        rs.getInt("minutos"), rs.getDouble("nota"), rs.getDate("fecha").toString,
        Option(rs.getString("clima")).getOrElse(""),
        Option(rs.getString("estadio")).getOrElse(""),
        Option(rs.getString("notas_partido")).getOrElse(""),
        Option(rs.getString("video_url")).getOrElse(""),
        Option(rs.getString("reaccion_goles")).getOrElse(""),
        rs.getString("status"),
        Option(rs.getString("tipo_partido")).getOrElse("TORNEO"),
        rs.getInt("pc_t"), rs.getInt("pc_ok"), rs.getInt("pl_t"), rs.getInt("pl_ok"),
        Option(rs.getString("analisis_voz")).getOrElse(""),
        Option(rs.getString("torneo_nombre")).getOrElse(""),
        Option(rs.getString("fase")).getOrElse(""),
        rs.getInt("paradas"), rs.getInt("paradas_1v1"), rs.getInt("paradas_aereas"),
        rs.getInt("acciones_pie"),
        Option(rs.getString("zona_tiros")).getOrElse(""),
        Option(rs.getString("zona_goles")).getOrElse("")
      )
    } finally { conn.close() }
    l
  }

  def getTournamentNames(): List[String] = {
    var l = List[String]()
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT DISTINCT torneo_nombre FROM matches WHERE torneo_nombre IS NOT NULL AND torneo_nombre != '' ORDER BY torneo_nombre"
      )
      while (rs.next()) l = l :+ rs.getString("torneo_nombre")
    } finally { conn.close() }
    l
  }

  def getSeasonEvolution(): List[(String, Double, Int, Int, Int)] = {
    // (anio, mediaPartidos, pj, gc, porteriasCero)
    var l = List[(String, Double, Int, Int, Int)]()
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("""
        SELECT
          EXTRACT(YEAR FROM fecha)::TEXT as anio,
          AVG(nota) as media,
          COUNT(*) as pj,
          SUM(goles_contra) as gc,
          SUM(CASE WHEN goles_contra = 0 THEN 1 ELSE 0 END) as pcs
        FROM matches WHERE status='PLAYED'
        GROUP BY EXTRACT(YEAR FROM fecha)
        ORDER BY anio ASC
      """)
      while (rs.next()) l = l :+ (
        rs.getString("anio"),
        rs.getDouble("media"),
        rs.getInt("pj"),
        rs.getInt("gc"),
        rs.getInt("pcs")
      )
    } finally { conn.close() }
    l
  }

  case class InjuryRecord(id: Int, fechaInicio: String, fechaAlta: String, zona: String, tipo: String, gravedad: String, descripcion: String, diasBaja: Int, activa: Boolean,
                           tipoClasificado: String, lado: String, causaProbable: String, partidosPerdidos: Int)

  def logInjury(zona: String, tipo: String, gravedad: String, desc: String,
                tipoClasificado: String = "OTRO", lado: String = "NA", causaProbable: String = "", partidosPerdidos: Int = 0): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("INSERT INTO injuries (zona, tipo, gravedad, descripcion, activa, tipo_clasificado, lado, causa_probable, partidos_perdidos) VALUES (?,?,?,?,true,?,?,?,?)")
      ps.setString(1, fixEncoding(zona)); ps.setString(2, fixEncoding(tipo))
      ps.setString(3, gravedad); ps.setString(4, fixEncoding(desc))
      ps.setString(5, tipoClasificado); ps.setString(6, lado); ps.setString(7, fixEncoding(causaProbable)); ps.setInt(8, partidosPerdidos)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def closeInjury(id: Int, fechaAlta: String, diasBaja: Int): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("UPDATE injuries SET activa=false, fecha_alta=?, dias_baja=? WHERE id=?")
      ps.setDate(1, java.sql.Date.valueOf(fechaAlta))
      ps.setInt(2, diasBaja); ps.setInt(3, id)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def getInjuries(): List[InjuryRecord] = {
    var l = List[InjuryRecord]()
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("SELECT * FROM injuries ORDER BY fecha_inicio DESC")
      while (rs.next()) l = l :+ InjuryRecord(
        rs.getInt("id"),
        rs.getDate("fecha_inicio").toString,
        Option(rs.getDate("fecha_alta")).map(_.toString).getOrElse(""),
        Option(rs.getString("zona")).getOrElse(""),
        Option(rs.getString("tipo")).getOrElse(""),
        Option(rs.getString("gravedad")).getOrElse("LEVE"),
        Option(rs.getString("descripcion")).getOrElse(""),
        rs.getInt("dias_baja"),
        rs.getBoolean("activa"),
        Option(rs.getString("tipo_clasificado")).getOrElse("OTRO"),
        Option(rs.getString("lado")).getOrElse("NA"),
        Option(rs.getString("causa_probable")).getOrElse(""),
        rs.getInt("partidos_perdidos")
      )
    } finally { conn.close() }
    l
  }

  def injuryTipoColor(tipoClasificado: String): String = tipoClasificado match {
    case "OSEA" | "APOFISITIS"      => "#dc3545"
    case "ARTICULAR" | "SOBREUSO"   => "#fd7e14"
    case "MUSCULAR"                  => "#ffc107"
    case "CONTUSION" | "ENFERMEDAD" => "#6c757d"
    case _                            => "#6c757d"
  }

  // Zonas con >=2 lesiones separadas por menos de 6 meses entre inicio de una y la siguiente
  def getZonasRecurrentes(): Set[String] = {
    val injuries = getInjuries()
    injuries.groupBy(_.zona.toLowerCase.trim).collect {
      case (zona, lst) if lst.size >= 2 =>
        val fechas = lst.flatMap(i => try Some(LocalDate.parse(i.fechaInicio)) catch { case _: Exception => None }).sorted
        val recurrente = fechas.sliding(2).exists { case Seq(a, b) => java.time.temporal.ChronoUnit.MONTHS.between(a, b) < 6 }
        (zona, recurrente)
    }.collect { case (zona, true) => zona }.toSet
  }

  // Lectura desde cache unicamente — nunca llama a Gemini en el render de pagina
  def getInjuryPatternAnalysisCached(): Option[String] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT payload FROM feature_cache WHERE cache_key = 'injury_patterns' AND updated_at > NOW() - INTERVAL '30 days'"
      )
      if (rs.next()) Some(ujson.read(rs.getString("payload"))("analisis").str) else None
    } finally { conn.close() }
  }

  // Llamada real a Gemini — SOLO se invoca desde el boton POST "Detectar patrones"
  def generateInjuryPatternAnalysis(): String = {
    val conn = getConnection()
    try {
      val injuries = getInjuries()
      if (injuries.isEmpty) return "Sin lesiones registradas todavía."

      val card = getLatestCardData()
      val edad = calcularEdadExacta(card.fechaNacimiento)
      val listaStr = injuries.map(i => s"${i.tipoClasificado} en ${i.zona} (${i.fechaInicio}, ${i.diasBaja} días de baja)").mkString("; ")

      val prompt = s"""Analiza el historial de lesiones de Héctor, portero de $edad años: [$listaStr]. Detecta: 1) Si hay alguna zona corporal recurrente, 2) Si hay patrón temporal (siempre en la misma época o tras carga alta), 3) Recomendación preventiva concreta para el padre y el entrenador. Texto plano, máximo 3 líneas por punto."""

      val analisis = AIProvider.ask(prompt, None, bypassCache = true)
      val payload = ujson.Obj("analisis" -> analisis)
      val upsert = conn.prepareStatement("""
        INSERT INTO feature_cache (cache_key, payload, updated_at) VALUES ('injury_patterns', ?, NOW())
        ON CONFLICT (cache_key) DO UPDATE SET payload = EXCLUDED.payload, updated_at = NOW()
      """)
      upsert.setString(1, ujson.write(payload))
      upsert.executeUpdate()
      analisis
    } finally { conn.close() }
  }

  def getCleanSheetPredictor(rival: String): Map[String, Any] = {
    val conn = getConnection()
    try {
      // Historial vs rival
      val psR = conn.prepareStatement("SELECT COUNT(*) as pj, SUM(CASE WHEN goles_contra=0 THEN 1 ELSE 0 END) as pcs, AVG(nota) as avg_nota FROM matches WHERE LOWER(rival) LIKE LOWER(?) AND status='PLAYED'")
      psR.setString(1, s"%$rival%")
      val rsR = psR.executeQuery()
      val (pj, pcs, avgNota) = if (rsR.next()) (rsR.getInt("pj"), rsR.getInt("pcs"), rsR.getDouble("avg_nota")) else (0, 0, 0.0)

      // Forma reciente (ultimos 5 partidos)
      // (agregar sobre una subconsulta: AVG(...) con ORDER BY fecha LIMIT 5 en la misma consulta es SQL invalido)
      val rsF = conn.createStatement().executeQuery("SELECT AVG(nota) as avg, SUM(CASE WHEN goles_contra=0 THEN 1 ELSE 0 END) as pcs, COUNT(*) as pj FROM (SELECT nota, goles_contra FROM matches WHERE status='PLAYED' ORDER BY fecha DESC LIMIT 5) ultimos")
      val (formaAvg, formaPcs, formaPj) = if (rsF.next()) (rsF.getDouble("avg"), rsF.getInt("pcs"), rsF.getInt("pj")) else (0.0, 0, 5)

      // Sueno ultima noche
      val rsS = conn.createStatement().executeQuery("SELECT horas_sueno FROM wellness ORDER BY fecha DESC LIMIT 1")
      val horasSueno = if (rsS.next()) rsS.getDouble("horas_sueno") else 0.0

      // ACWR inline
      val rsAc = conn.prepareStatement("SELECT COALESCE(SUM(rpe*60),0) FROM trainings WHERE fecha >= CURRENT_DATE - ?")
      rsAc.setInt(1, 7); val rsAcR = rsAc.executeQuery()
      val acuteLoad = if (rsAcR.next()) rsAcR.getDouble(1) else 0.0
      val rsCh = conn.prepareStatement("SELECT COALESCE(SUM(rpe*60),0) FROM trainings WHERE fecha >= CURRENT_DATE - ?")
      rsCh.setInt(1, 28); val rsChR = rsCh.executeQuery()
      val chronicLoad = if (rsChR.next()) rsChR.getDouble(1) else 0.0
      val acwr = if (chronicLoad > 0) (acuteLoad / 7.0) / (chronicLoad / 28.0) else 1.0

      // Calcular probabilidad (heuristica)
      var score = 50.0
      if (pj > 0)       score += (pcs.toDouble / pj) * 20
      if (formaPj > 0)  score += (formaPcs.toDouble / formaPj) * 15
      if (formaAvg > 7) score += 10 else if (formaAvg > 5) score += 5
      if (horasSueno >= 8) score += 8 else if (horasSueno < 6 && horasSueno > 0) score -= 8
      if (acwr > 1.5)   score -= 10 else if (acwr < 0.8) score -= 5
      val prob = math.min(95, math.max(5, score.toInt))

      Map("prob" -> prob, "pj" -> pj, "pcs" -> pcs, "avgNota" -> avgNota,
          "formaAvg" -> formaAvg, "horasSueno" -> horasSueno, "acwr" -> acwr)
    } finally { conn.close() }
  }

  def getGloveThermostat(): (String, String, String) = {
    // (recomendacion, motivo, latex_type)
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT clima, temperatura FROM matches WHERE status='PLAYED' ORDER BY fecha DESC LIMIT 5"
      )
      var temps = List[Int](); var climas = List[String]()
      while (rs.next()) {
        temps = temps :+ rs.getInt("temperatura")
        climas = climas :+ Option(rs.getString("clima")).getOrElse("Sol")
      }
      val avgTemp = if (temps.nonEmpty) temps.sum / temps.size else 15
      val hasRain = climas.count(c => c.contains("Lluvia") || c.contains("lluvia")) > 0
      val (latex, motivo) = if (avgTemp <= 5) ("Latex Hibrido Frio", s"Temperatura media ${avgTemp}C — latex hibrido mantiene agarre")
                            else if (avgTemp <= 12) ("Latex Soft Grip", s"Temperatura fresca ${avgTemp}C — latex blando optimo")
                            else if (hasRain) ("Latex Aqua", "Condiciones humedas recientes — latex aqua recomendado")
                            else if (avgTemp >= 25) ("Latex Duo Soft", s"Temperatura alta ${avgTemp}C — latex suave con respiro")
                            else ("Latex Contact", s"Condiciones ideales ${avgTemp}C — latex contact estandar")
      val gloves = conn.createStatement().executeQuery("SELECT nombre FROM gear WHERE tipo='Guantes' AND activo=TRUE LIMIT 1")
      val gloveName = if (gloves.next()) gloves.getString("nombre") else "Sin guantes registrados"
      (latex, motivo, gloveName)
    } finally { conn.close() }
  }

  def getFlashCardData(rival: String): Map[String, Any] = {
    val conn = getConnection()
    try {
      // Historial vs rival
      val ps = conn.prepareStatement("SELECT goles_favor, goles_contra, nota, fecha, zona_goles, notas_partido FROM matches WHERE LOWER(rival) LIKE LOWER(?) AND status='PLAYED' ORDER BY fecha DESC LIMIT 5")
      ps.setString(1, s"%$rival%")
      val rs = ps.executeQuery()
      var partidos = List[(String,Double,String,String,String)]()
      while (rs.next()) {
        val res = s"${rs.getInt(1)}-${rs.getInt(2)}"
        partidos = partidos :+ (res, rs.getDouble(3), rs.getString(4),
          Option(rs.getString(5)).getOrElse(""), Option(rs.getString(6)).getOrElse(""))
      }

      // Zona mas vulnerable vs ese rival
      val zonasCombinadas = partidos.flatMap(_._4.split(",").filter(_.nonEmpty))
      val zonaMasVulnerable = if (zonasCombinadas.nonEmpty) zonasCombinadas.groupBy(identity).maxBy(_._2.size)._1 else "—"

      // Clips de paradas recientes
      val rsV = conn.createStatement().executeQuery(
        "SELECT vt.minuto, vt.segundo, vt.tipo, m.rival, m.video_url FROM video_tags vt JOIN matches m ON vt.match_id=m.id WHERE vt.tipo='PARADA' AND m.video_url IS NOT NULL ORDER BY m.fecha DESC LIMIT 3"
      )
      var clips = List[(Int,Int,String,String,String)]()
      while (rsV.next()) clips = clips :+ (rsV.getInt(1), rsV.getInt(2), rsV.getString(3), rsV.getString(4), rsV.getString(5))

      // Rival info
      val rsRi = conn.prepareStatement("SELECT estilo_juego, jugadores_clave FROM rivals WHERE LOWER(nombre) LIKE LOWER(?)")
      rsRi.setString(1, s"%$rival%")
      val rsRiR = rsRi.executeQuery()
      val (estilo, claves) = if (rsRiR.next()) (Option(rsRiR.getString("estilo_juego")).getOrElse(""), Option(rsRiR.getString("jugadores_clave")).getOrElse("")) else ("", "")

      Map("partidos" -> partidos, "zonaMasVulnerable" -> zonaMasVulnerable,
          "clips" -> clips, "estilo" -> estilo, "claves" -> claves)
    } finally { conn.close() }
  }

  // ── GK INFLUENCE ANALYTICS ────────────────────────────────────────────────
  // BLOQUE B3: seasonId=0 = historico completo (comportamiento anterior, sin cambios para llamadas existentes)
  def getGKInfluenceStats(seasonId: Int = 0): Map[String, Any] = {
    val conn = getConnection()
    try {
      val sf = seasonFilter(seasonId)
      // Distribuciones con pie (acciones_pie) y resultado posterior
      val rs = conn.createStatement().executeQuery(s"""
        SELECT
          AVG(acciones_pie) as avg_pie,
          SUM(acciones_pie) as total_pie,
          AVG(CASE WHEN acciones_pie > 5 THEN nota ELSE NULL END) as nota_alta_pie,
          AVG(CASE WHEN acciones_pie <= 5 THEN nota ELSE NULL END) as nota_baja_pie,
          AVG(CASE WHEN pc_t > 0 THEN CAST(pc_ok AS FLOAT)/pc_t ELSE NULL END) as pct_centros,
          AVG(CASE WHEN pl_t > 0 THEN CAST(pl_ok AS FLOAT)/pl_t ELSE NULL END) as pct_largos,
          COUNT(*) as pj,
          SUM(CASE WHEN goles_contra = 0 THEN 1 ELSE 0 END) as pcs,
          AVG(nota) as avg_nota
        FROM matches WHERE status='PLAYED' $sf
      """)
      if (!rs.next()) return Map.empty

      val avgPie      = rs.getDouble("avg_pie")
      val totalPie    = rs.getInt("total_pie")
      val notaAltaPie = rs.getDouble("nota_alta_pie")
      val notaBajaPie = rs.getDouble("nota_baja_pie")
      val pctCentros  = rs.getDouble("pct_centros")
      val pctLargos   = rs.getDouble("pct_largos")
      val pj          = rs.getInt("pj")
      val pcs         = rs.getInt("pcs")
      val avgNota     = rs.getDouble("avg_nota")

      // Serie temporal: acciones_pie + nota por partido (ultimos 20)
      val rsSerie = conn.createStatement().executeQuery(
        s"SELECT fecha::TEXT, acciones_pie, nota, rival FROM matches WHERE status='PLAYED' $sf ORDER BY fecha DESC LIMIT 20"
      )
      var serie = List[(String, Int, Double, String)]()
      while (rsSerie.next()) serie = serie :+ (
        rsSerie.getString(1), rsSerie.getInt(2), rsSerie.getDouble(3), rsSerie.getString(4)
      )

      // Distribucion por tipo de balon parado
      val rsTipo = conn.createStatement().executeQuery(s"""
        SELECT
          SUM(pc_t) as cent_total, SUM(pc_ok) as cent_ok,
          SUM(pl_t) as larg_total, SUM(pl_ok) as larg_ok
        FROM matches WHERE status='PLAYED' $sf
      """)
      val (centTotal, centOk, largTotal, largOk) = if (rsTipo.next())
        (rsTipo.getInt(1), rsTipo.getInt(2), rsTipo.getInt(3), rsTipo.getInt(4))
      else (0, 0, 0, 0)

      Map(
        "avgPie" -> avgPie, "totalPie" -> totalPie,
        "notaAltaPie" -> notaAltaPie, "notaBajaPie" -> notaBajaPie,
        "pctCentros" -> pctCentros, "pctLargos" -> pctLargos,
        "pj" -> pj, "pcs" -> pcs, "avgNota" -> avgNota,
        "serie" -> serie,
        "centTotal" -> centTotal, "centOk" -> centOk,
        "largTotal" -> largTotal, "largOk" -> largOk
      )
    } finally { conn.close() }
  }

  // ── BIOMECANICA POSICIONAL ─────────────────────────────────────────────────
  def getBiomecPosicional(seasonId: Int = 0): Map[String, Any] = {
    val conn = getConnection()
    try {
      // Zonas de gol encajado vs zonas de parada (9 zonas: TL,TC,TR,ML,MC,MR,BL,BC,BR)
      val zones = Seq("TL","TC","TR","ML","MC","MR","BL","BC","BR")
      val golesMap  = scala.collection.mutable.Map(zones.map(_ -> 0): _*)
      val paradasMap= scala.collection.mutable.Map(zones.map(_ -> 0): _*)
      val tirosMap  = scala.collection.mutable.Map(zones.map(_ -> 0): _*)

      val rs = conn.createStatement().executeQuery(
        s"SELECT zona_goles, zona_paradas, zona_tiros FROM matches WHERE status='PLAYED' ${seasonFilter(seasonId)}"
      )
      while (rs.next()) {
        Option(rs.getString("zona_goles")).getOrElse("").split(",").filter(_.nonEmpty).foreach { z =>
          val k = z.trim.toUpperCase; if (golesMap.contains(k)) golesMap(k) += 1
        }
        Option(rs.getString("zona_paradas")).getOrElse("").split(",").filter(_.nonEmpty).foreach { z =>
          val k = z.trim.toUpperCase; if (paradasMap.contains(k)) paradasMap(k) += 1
        }
        Option(rs.getString("zona_tiros")).getOrElse("").split(",").filter(_.nonEmpty).foreach { z =>
          val k = z.trim.toUpperCase; if (tirosMap.contains(k)) tirosMap(k) += 1
        }
      }

      // Puntos ciegos: zonas donde goles > paradas (vulnerables)
      val puntosCiegos = zones.filter(z => golesMap(z) > paradasMap(z)).sortBy(-golesMap(_))

      // Zonas fuertes: zonas donde paradas > goles
      val zonasFuertes = zones.filter(z => paradasMap(z) > golesMap(z)).sortBy(-paradasMap(_))

      // Eficiencia por zona: paradas / (paradas + goles)
      val eficiencia = zones.map { z =>
        val total = paradasMap(z) + golesMap(z)
        val pct = if (total > 0) (paradasMap(z).toDouble / total * 100).toInt else -1
        z -> pct
      }.toMap

      // Porcentaje de stop por zona de tiro
      val stopRate = zones.map { z =>
        val goles = golesMap(z); val paradas = paradasMap(z)
        val total = goles + paradas
        z -> (if (total > 0) f"${paradas.toDouble/total*100}%.0f%%" else "—")
      }.toMap

      Map(
        "goles" -> golesMap.toMap, "paradas" -> paradasMap.toMap,
        "tiros" -> tirosMap.toMap, "eficiencia" -> eficiencia,
        "puntosCiegos" -> puntosCiegos, "zonasFuertes" -> zonasFuertes,
        "stopRate" -> stopRate, "zones" -> zones
      )
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE C — SET-PIECE CONTROL (SQL puro, sin Gemini)
  // ─────────────────────────────────────────────────────────────────────────────
  def getSetPieceStats(seasonId: Int = 0): Map[String, Any] = {
    val conn = getConnection()
    try {
      val sf = seasonFilter(seasonId)
      val rs = conn.createStatement().executeQuery(
        s"SELECT fecha, nota, corners_dominados, corners_cedidos, faltas_area_dominadas FROM matches WHERE status='PLAYED' $sf ORDER BY fecha ASC")
      case class SP(fecha: String, nota: Double, dominados: Int, cedidos: Int, faltas: Int)
      var rows = List[SP]()
      while (rs.next()) rows = rows :+ SP(
        rs.getString("fecha"), rs.getDouble("nota"),
        rs.getInt("corners_dominados"), rs.getInt("corners_cedidos"), rs.getInt("faltas_area_dominadas")
      )

      // Solo partidos con algun dato de balon parado registrado
      val conDatos = rows.filter(r => r.dominados > 0 || r.cedidos > 0)
      val totalDominados = conDatos.map(_.dominados).sum
      val totalCedidos = conDatos.map(_.cedidos).sum
      val totalFaltas = conDatos.map(_.faltas).sum
      val ratioDominio = if (totalDominados + totalCedidos > 0) totalDominados.toDouble / (totalDominados + totalCedidos) * 100 else 0.0

      // Tendencia: primera mitad de temporada vs segunda mitad (por orden cronologico)
      val n = conDatos.size
      val (primeraMitad, segundaMitad) = conDatos.splitAt(n / 2)
      def ratioDe(l: List[SP]): Double = {
        val d = l.map(_.dominados).sum; val c = l.map(_.cedidos).sum
        if (d + c > 0) d.toDouble / (d + c) * 100 else 0.0
      }
      val ratioPrimera = ratioDe(primeraMitad)
      val ratioSegunda = ratioDe(segundaMitad)
      val tendencia =
        if (n < 4) "SIN_DATOS_SUFICIENTES"
        else if (ratioSegunda > ratioPrimera + 5) "MEJORANDO"
        else if (ratioSegunda < ratioPrimera - 5) "EMPEORANDO"
        else "ESTABLE"

      // Correlacion con nota: nota media cuando domina el area (>=70% ese partido) vs cuando no
      def ratioPartido(r: SP): Double = if (r.dominados + r.cedidos > 0) r.dominados.toDouble / (r.dominados + r.cedidos) else -1.0
      val partidosDominio = conDatos.filter(r => ratioPartido(r) >= 0.7)
      val partidosNoDominio = conDatos.filter(r => { val rp = ratioPartido(r); rp >= 0 && rp < 0.7 })
      val notaMediaDominio = if (partidosDominio.nonEmpty) partidosDominio.map(_.nota).sum / partidosDominio.size else 0.0
      val notaMediaNoDominio = if (partidosNoDominio.nonEmpty) partidosNoDominio.map(_.nota).sum / partidosNoDominio.size else 0.0

      // Serie para grafico de evolucion del ratio de dominio por partido
      val serieFechas = conDatos.map(_.fecha.take(10))
      val serieRatios = conDatos.map(r => { val rp = ratioPartido(r); if (rp < 0) 0.0 else rp * 100 })

      Map(
        "totalDominados" -> totalDominados, "totalCedidos" -> totalCedidos, "totalFaltas" -> totalFaltas,
        "ratioDominio" -> ratioDominio, "tendencia" -> tendencia,
        "ratioPrimera" -> ratioPrimera, "ratioSegunda" -> ratioSegunda,
        "notaMediaDominio" -> notaMediaDominio, "notaMediaNoDominio" -> notaMediaNoDominio,
        "nPartidosConDatos" -> conDatos.size,
        "serieFechas" -> serieFechas, "serieRatios" -> serieRatios
      )
    } finally { conn.close() }
  }

  // ── EMOTIONAL INTELLIGENCE ENGINE ─────────────────────────────────────────
  case class EmotionalEntry(fecha: String, animo: Int, energia: Int, notas: String,
                             notaPartido: Option[Double], reaccionGoles: String)

  def getEmotionalData(): Map[String, Any] = {
    val conn = getConnection()
    try {
      // Serie de 45 dias: animo + energia + notas conducta + nota partido ese dia
      val rs = conn.createStatement().executeQuery("""
        SELECT
          w.fecha::TEXT,
          COALESCE(w.animo, 3)    as animo,
          COALESCE(w.energia, 3)  as energia,
          COALESCE(w.notas_conducta, '') as notas,
          m.nota                  as nota_partido,
          COALESCE(m.reaccion_goles, '') as reaccion
        FROM wellness w
        LEFT JOIN matches m ON m.fecha = w.fecha AND m.status = 'PLAYED'
        WHERE w.fecha >= CURRENT_DATE - 45
        ORDER BY w.fecha ASC
      """)
      var entries = List[EmotionalEntry]()
      while (rs.next()) {
        val notaOpt = { val v = rs.getDouble("nota_partido"); if (rs.wasNull()) None else Some(v) }
        entries = entries :+ EmotionalEntry(
          rs.getString("fecha"), rs.getInt("animo"), rs.getInt("energia"),
          Option(rs.getString("notas")).getOrElse(""), notaOpt,
          Option(rs.getString("reaccion")).getOrElse("")
        )
      }

      // Correlacion animo -> nota partido
      val conPartido = entries.filter(_.notaPartido.isDefined)
      val correlacion: Double = if (conPartido.size >= 3) {
        val animoAlto  = conPartido.filter(_.animo >= 4).flatMap(_.notaPartido)
        val animoBajo  = conPartido.filter(_.animo <= 2).flatMap(_.notaPartido)
        val diffCorr = (if (animoAlto.nonEmpty) animoAlto.sum / animoAlto.size else 0.0) -
                       (if (animoBajo.nonEmpty) animoBajo.sum / animoBajo.size else 0.0)
        diffCorr
      } else 0.0

      // Patron emocional: dias bajos consecutivos (riesgo burnout)
      val diasBajosConsecutivos = {
        var maxRacha = 0; var racha = 0
        entries.foreach { e =>
          if (e.animo <= 2 || e.energia <= 2) { racha += 1; maxRacha = math.max(maxRacha, racha) }
          else racha = 0
        }
        maxRacha
      }

      // Estado emocional actual (ultimos 7 dias)
      val recientes = entries.takeRight(7)
      val avgAnimoReciente  = if (recientes.nonEmpty) recientes.map(_.animo.toDouble).sum / recientes.size else 3.0
      val avgEnergiaReciente= if (recientes.nonEmpty) recientes.map(_.energia.toDouble).sum / recientes.size else 3.0

      // Notas de conducta recientes para analisis IA
      val notasParaIA = entries.takeRight(14)
        .filter(_.notas.nonEmpty)
        .map(e => s"${e.fecha}: ${e.notas}")
        .mkString(" | ")

      // Reacciones a goles encajados
      val reacciones = entries.filter(_.reaccionGoles.nonEmpty).map(_.reaccionGoles).take(5)

      // Score de resiliencia emocional (0-100)
      val resilienciaScore = {
        var s = 50.0
        if (correlacion > 1.0) s += 15 else if (correlacion > 0.5) s += 8
        if (diasBajosConsecutivos == 0) s += 15 else if (diasBajosConsecutivos <= 2) s += 5 else s -= 10
        if (avgAnimoReciente >= 4) s += 10 else if (avgAnimoReciente <= 2) s -= 10
        if (avgEnergiaReciente >= 4) s += 10 else if (avgEnergiaReciente <= 2) s -= 5
        math.min(99, math.max(1, s.toInt))
      }

      // Analisis IA de las notas emocionales (bypassCache para siempre tener fresco)
      val analisisIA: String = if (notasParaIA.nonEmpty) {
        val prompt = s"""Eres un psicopedagogo deportivo analizando el diario emocional de Hector, portero de 9 anos.
Entradas recientes (fecha: nota): $notasParaIA
Reacciones a goles encajados: ${reacciones.mkString(" | ")}
Proporciona un analisis BREVE en 3 partes:
PATRON: [patron emocional detectado en 1 frase]
FORTALEZA: [principal fortaleza mental en 1 frase]
CONSEJO: [1 consejo practico concreto para esta semana]
Responde en espanol, tono positivo y motivador para un nino."""
        AIProvider.ask(prompt, None, bypassCache = true)
      } else "Sin suficientes notas de conducta para el analisis. Registra tu estado diario para activar este modulo."

      Map(
        "entries"     -> entries,
        "correlacion" -> correlacion,
        "diasBajosConsecutivos" -> diasBajosConsecutivos,
        "avgAnimoReciente"  -> avgAnimoReciente,
        "avgEnergiaReciente"-> avgEnergiaReciente,
        "resilienciaScore"  -> resilienciaScore,
        "analisisIA"        -> analisisIA,
        "notasCount"        -> entries.count(_.notas.nonEmpty),
        "totalEntries"      -> entries.size
      )
    } finally { conn.close() }
  }


  // == DIGITAL TWIN HECTOR 2035 ==============================================

  // == MATCH GOALS: Contexto de goles encajados ============================
  def saveMatchGoal(matchId: Int, minuto: Int, origen: String, situacion: String,
                    responsabilidad: String, eraParable: String, zonaGol: String, notas: String): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement(
        "INSERT INTO match_goals (match_id, minuto, origen, situacion, responsabilidad, era_parable, zona_gol, notas) " +
        "VALUES (?,?,?,?,?,?,?,?)")
      ps.setInt(1, matchId); ps.setInt(2, minuto)
      ps.setString(3, fixEncoding(origen)); ps.setString(4, fixEncoding(situacion))
      ps.setString(5, responsabilidad); ps.setString(6, eraParable)
      ps.setString(7, zonaGol); ps.setString(8, fixEncoding(notas))
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def deleteMatchGoals(matchId: Int): Unit = {
    val conn = getConnection()
    try { conn.createStatement().executeUpdate(s"DELETE FROM match_goals WHERE match_id = $matchId") }
    finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE 4.1 / 4.3 — RUBRICA DE VALORACION Y METRICAS DE CANTERA
  // ─────────────────────────────────────────────────────────────────────────────
  def updateMatchExtras(matchId: Int,
                         rubricaPosicion: Option[Int], rubricaDecisiones: Option[Int], rubricaPies: Option[Int],
                         rubricaComunicacion: Option[Int], rubricaActitud: Option[Int],
                         posicionSet: String, alturaBloque: String, pieNoDominanteAcciones: Int, iniciativaVocal: String,
                         // BLOQUE B: autopercepcion pre-partido de Hector (1-5)
                         autopercepcionPrepartido: Option[Int] = None,
                         // BLOQUE C: contexto avanzado del partido
                         calentamientoMin: Option[Int] = None, calentamientoTipo: String = "",
                         superficie: String = "", factoresExternos: String = "",
                         velocidadDistribucion: String = "", economiaMovimiento: Option[Int] = None,
                         calidadDecisionPct: Option[Int] = None,
                         // BLOQUE B: autoevaluacion privada de la conducta del padre en la banda (1-5)
                         conductaPadre: Option[Int] = None,
                         // BLOQUE G: efectividad del scanning (amplia scanning_rate)
                         scanningEfectivo: Int = 0,
                         // BLOQUE H: exito en 1v1 por angulo de entrada (JSON)
                         angulo1v1Data: String = "",
                         // BLOQUE O: rutina pre-partido (SI/NO/SIN_RUTINA)
                         rutinaPrepartido: String = "",
                         // BLOQUE S: regulacion emocional tras el gol mas importante
                         regulacionEmocional: String = ""): Unit = {
    if (matchId <= 0) return
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("""
        UPDATE matches SET
          rubrica_posicion=?, rubrica_decisiones=?, rubrica_pies=?, rubrica_comunicacion=?, rubrica_actitud=?,
          posicion_set=?, altura_bloque=?, pie_no_dominante_acciones=?, iniciativa_vocal=?,
          autopercepcion_prepartido=COALESCE(?, autopercepcion_prepartido),
          calentamiento_min=?, calentamiento_tipo=?, superficie=?, factores_externos=?,
          velocidad_distribucion=?, economia_movimiento=?, calidad_decision_pct=?,
          conducta_padre=COALESCE(?, conducta_padre),
          scanning_efectivo=?,
          angulo_1v1_data=COALESCE(NULLIF(?, ''), angulo_1v1_data),
          rutina_prepartido=COALESCE(NULLIF(?, ''), rutina_prepartido),
          regulacion_emocional=COALESCE(NULLIF(?, ''), regulacion_emocional)
        WHERE id=?""")
      def setOptInt(idx: Int, v: Option[Int]): Unit = v match { case Some(x) => ps.setInt(idx, x); case None => ps.setNull(idx, java.sql.Types.INTEGER) }
      setOptInt(1, rubricaPosicion); setOptInt(2, rubricaDecisiones); setOptInt(3, rubricaPies)
      setOptInt(4, rubricaComunicacion); setOptInt(5, rubricaActitud)
      if (posicionSet.nonEmpty) ps.setString(6, posicionSet) else ps.setNull(6, java.sql.Types.VARCHAR)
      if (alturaBloque.nonEmpty) ps.setString(7, alturaBloque) else ps.setNull(7, java.sql.Types.VARCHAR)
      ps.setInt(8, pieNoDominanteAcciones)
      if (iniciativaVocal.nonEmpty) ps.setString(9, iniciativaVocal) else ps.setNull(9, java.sql.Types.VARCHAR)
      setOptInt(10, autopercepcionPrepartido)
      setOptInt(11, calentamientoMin)
      if (calentamientoTipo.nonEmpty) ps.setString(12, calentamientoTipo) else ps.setNull(12, java.sql.Types.VARCHAR)
      if (superficie.nonEmpty) ps.setString(13, superficie) else ps.setNull(13, java.sql.Types.VARCHAR)
      if (factoresExternos.nonEmpty) ps.setString(14, fixEncoding(factoresExternos)) else ps.setNull(14, java.sql.Types.VARCHAR)
      if (velocidadDistribucion.nonEmpty) ps.setString(15, velocidadDistribucion) else ps.setNull(15, java.sql.Types.VARCHAR)
      setOptInt(16, economiaMovimiento)
      setOptInt(17, calidadDecisionPct)
      setOptInt(18, conductaPadre)
      ps.setInt(19, scanningEfectivo)
      ps.setString(20, angulo1v1Data)
      ps.setString(21, rutinaPrepartido)
      ps.setString(22, regulacionEmocional)
      ps.setInt(23, matchId)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE C — CALENDARIO DE CARGA COGNITIVA ESCOLAR
  // ─────────────────────────────────────────────────────────────────────────────
  def saveCalendarioEscolar(fechaInicio: String, fechaFin: String, tipo: String, descripcion: String): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement(
        "INSERT INTO calendario_escolar (fecha_inicio, fecha_fin, tipo, descripcion) VALUES (?::date, ?::date, ?, ?)")
      ps.setString(1, fechaInicio); ps.setString(2, fechaFin); ps.setString(3, tipo); ps.setString(4, fixEncoding(descripcion))
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def getCalendarioEscolar(): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT id, fecha_inicio, fecha_fin, tipo, descripcion FROM calendario_escolar ORDER BY fecha_inicio DESC")
      var l = List[Map[String, Any]]()
      while (rs.next()) l = l :+ Map(
        "id" -> rs.getInt("id"), "fechaInicio" -> rs.getDate("fecha_inicio").toString, "fechaFin" -> rs.getDate("fecha_fin").toString,
        "tipo" -> rs.getString("tipo"), "descripcion" -> fixEncoding(Option(rs.getString("descripcion")).getOrElse(""))
      )
      l
    } finally { conn.close() }
  }

  def deleteCalendarioEscolar(id: Int): Unit = {
    val conn = getConnection()
    try { val ps = conn.prepareStatement("DELETE FROM calendario_escolar WHERE id=?"); ps.setInt(1, id); ps.executeUpdate() }
    finally { conn.close() }
  }

  /** Tipo de periodo escolar (EXAMENES/TRIMESTRE_FIN/...) que cubre una fecha concreta, si lo hay. */
  private def periodoEscolarEnFecha(conn: Connection, fecha: String): Option[String] = {
    val ps = conn.prepareStatement("SELECT tipo FROM calendario_escolar WHERE ?::date >= fecha_inicio AND ?::date <= fecha_fin ORDER BY id DESC LIMIT 1")
    ps.setString(1, fecha); ps.setString(2, fecha)
    val rs = ps.executeQuery()
    if (rs.next()) Some(rs.getString("tipo")) else None
  }

  def getPeriodoEscolarHoy(): Option[String] = {
    val conn = getConnection()
    try periodoEscolarEnFecha(conn, LocalDate.now().toString) finally conn.close()
  }

  /** True si esa fecha cae en un periodo de EXAMENES — para el badge del historial de partidos. */
  def esFechaDeExamenes(fecha: String): Boolean = {
    val conn = getConnection()
    try periodoEscolarEnFecha(conn, fecha).contains("EXAMENES") finally conn.close()
  }

  /** True si algun dia entre hoy y los proximos `dias` cae en EXAMENES o TRIMESTRE_FIN. */
  def haySemanaEscolarCargada(dias: Int = 7): Boolean = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("""
        SELECT 1 FROM calendario_escolar
        WHERE tipo IN ('EXAMENES','TRIMESTRE_FIN')
          AND fecha_inicio <= CURRENT_DATE + ? AND fecha_fin >= CURRENT_DATE
        LIMIT 1
      """)
      ps.setInt(1, dias)
      ps.executeQuery().next()
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE D — DETECTOR DE JETLAG SOCIAL
  // ─────────────────────────────────────────────────────────────────────────────
  // SQL puro — sin Gemini.
  def detectarJetlagSocial(): Option[String] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("""
        SELECT
          AVG(CASE WHEN EXTRACT(DOW FROM fecha) IN (1,2,3,4,5) THEN horas_sueno END) as media_semana,
          AVG(CASE WHEN EXTRACT(DOW FROM fecha) IN (0,6) THEN horas_sueno END) as media_finde,
          STDDEV(horas_sueno) as variabilidad,
          COUNT(*) as n
        FROM wellness
        WHERE fecha >= CURRENT_DATE - INTERVAL '28 days' AND horas_sueno > 0
      """)
      if (!rs.next() || rs.getInt("n") < 14) return None
      val mediaSemana = rs.getDouble("media_semana")
      val semanaNula = rs.wasNull()
      val mediaFinde = rs.getDouble("media_finde")
      val findeNulo = rs.wasNull()
      if (semanaNula || findeNulo || mediaSemana <= 0 || mediaFinde <= 0) return None
      val diff = mediaFinde - mediaSemana
      if (math.abs(diff) > 1.5)
        Some(f"⏰ Patrón de jetlag social detectado: $mediaSemana%.1fh entre semana vs $mediaFinde%.1fh el fin de semana. Esta inconsistencia puede desajustar el reloj biológico y afectar el rendimiento del sábado.")
      else None
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE E — DETECTOR DE CONDICIONES DE RENDIMIENTO PICO
  // ─────────────────────────────────────────────────────────────────────────────
  // SQL puro — analiza los 10 partidos con mayor CPI o nota. Requiere >=15 partidos con Indice de Forma.
  def getCondicionesPico(): Map[String, Any] = {
    val conn = getConnection()
    try {
      val rsN = conn.createStatement().executeQuery(s"""
        SELECT COUNT(*) as n FROM matches m
        LEFT JOIN forma_diaria f ON f.fecha = m.fecha::date
        WHERE m.status = 'PLAYED' AND f.indice_forma IS NOT NULL
          ${seasonFilterActual("m")}
      """)
      val n = if (rsN.next()) rsN.getInt("n") else 0
      if (n < 15) return Map("suficiente" -> false, "n" -> n)

      val rs = conn.createStatement().executeQuery(s"""
        SELECT AVG(f.indice_forma) as forma_media_pico,
          AVG(f.sueno_score) as sueno_pico,
          AVG(f.acwr_score) as acwr_pico,
          AVG(f.descanso_score) as descanso_pico,
          MODE() WITHIN GROUP (ORDER BY m.clima) as clima_frecuente,
          MODE() WITHIN GROUP (ORDER BY m.es_local::text) as sede_frecuente,
          AVG(m.autopercepcion_prepartido) as autopercepcion_pico
        FROM (
          SELECT * FROM matches
          WHERE status = 'PLAYED' ${seasonFilterActual()}
          ORDER BY COALESCE(cpi, nota) DESC LIMIT 10
        ) m
        LEFT JOIN forma_diaria f ON f.fecha = m.fecha::date
      """)
      if (!rs.next()) return Map("suficiente" -> false, "n" -> n)
      Map(
        "suficiente" -> true, "n" -> n,
        "formaMediaPico" -> rs.getDouble("forma_media_pico"),
        "suenoPico" -> rs.getDouble("sueno_pico"),
        "acwrPico" -> rs.getDouble("acwr_pico"),
        "descansoPico" -> rs.getDouble("descanso_pico"),
        "climaFrecuente" -> Option(rs.getString("clima_frecuente")).getOrElse(""),
        "sedeFrecuente" -> Option(rs.getString("sede_frecuente")).getOrElse(""),
        "autopercepcionPico" -> rs.getDouble("autopercepcion_pico")
      )
    } finally { conn.close() }
  }

  /** Compara las condiciones de HOY con el perfil de condiciones pico. None si no hay perfil suficiente. */
  def getCoincidenciaConCondicionesPico(): Option[Int] = {
    val pico = getCondicionesPico()
    if (!pico("suficiente").asInstanceOf[Boolean]) return None
    val hoy = calcularFormaHoy()
    val suenoHoy = hoy("suenoScore").asInstanceOf[Double]
    val acwrHoy = hoy("acwrScore").asInstanceOf[Double]
    val descansoHoy = hoy("descansoScore").asInstanceOf[Double]
    val suenoPico = pico("suenoPico").asInstanceOf[Double]
    val acwrPico = pico("acwrPico").asInstanceOf[Double]
    val descansoPico = pico("descansoPico").asInstanceOf[Double]
    val distanciaMedia = (math.abs(suenoHoy - suenoPico) + math.abs(acwrHoy - acwrPico) + math.abs(descansoHoy - descansoPico)) / 3.0
    val coincidencia = math.max(0, math.min(100, (100 - distanciaMedia * 10).toInt))
    Some(coincidencia)
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE F — TRACKER CORRECCION PASO NEGATIVO (amplia /biomecanica)
  // ─────────────────────────────────────────────────────────────────────────────
  // SQL puro — usa posicion_set, que ya existe en matches. Requiere >=8 goles con dato.
  def getPasoNegativoTrend(seasonId: Int = 0): Map[String, Any] = {
    val conn = getConnection()
    try {
      val sf = seasonFilter(if (seasonId > 0) seasonId else 0)
      val rsTotal = conn.createStatement().executeQuery(
        s"SELECT COUNT(*) as n FROM matches WHERE status='PLAYED' AND posicion_set IS NOT NULL $sf")
      val total = if (rsTotal.next()) rsTotal.getInt("n") else 0
      if (total < 8) return Map("suficiente" -> false, "n" -> total)

      val rs = conn.createStatement().executeQuery(s"""
        SELECT EXTRACT(MONTH FROM fecha) as mes,
          COUNT(*) as goles_con_data,
          SUM(CASE WHEN posicion_set = 'PASO_NEGATIVO' THEN 1 ELSE 0 END) as paso_negativo,
          ROUND(100.0 * SUM(CASE WHEN posicion_set = 'PASO_NEGATIVO' THEN 1 ELSE 0 END)
            / NULLIF(COUNT(*), 0)) as pct_paso_negativo
        FROM matches
        WHERE status = 'PLAYED' AND posicion_set IS NOT NULL $sf
        GROUP BY EXTRACT(MONTH FROM fecha)
        ORDER BY mes
      """)
      var serie = List[Map[String, Any]]()
      while (rs.next()) serie = serie :+ Map(
        "mes" -> rs.getInt("mes"), "golesConData" -> rs.getInt("goles_con_data"),
        "pasoNegativo" -> rs.getInt("paso_negativo"), "pctPasoNegativo" -> rs.getInt("pct_paso_negativo")
      )
      val tendencia =
        if (serie.size < 2) "SIN_TENDENCIA"
        else if (serie.last("pctPasoNegativo").asInstanceOf[Int] < serie.head("pctPasoNegativo").asInstanceOf[Int]) "CORRIGIENDO"
        else "ESTABLE_O_PEOR"
      Map("suficiente" -> true, "n" -> total, "serie" -> serie, "tendencia" -> tendencia)
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE H — EXITO EN 1V1 POR ANGULO DE ENTRADA
  // ─────────────────────────────────────────────────────────────────────────────
  // SQL/JSON puro — sin Gemini. Requiere >=15 acciones 1v1 con angulo.
  def get1v1ByAngulo(seasonId: Int = 0): Map[String, Any] = {
    val conn = getConnection()
    try {
      val sf = seasonFilter(seasonId)
      val rs = conn.createStatement().executeQuery(
        s"SELECT angulo_1v1_data FROM matches WHERE status='PLAYED' AND angulo_1v1_data IS NOT NULL AND angulo_1v1_data != '' $sf")
      var okC = 0; var gcC = 0; var okI = 0; var gcI = 0; var okD = 0; var gcD = 0
      while (rs.next()) {
        try {
          val j = ujson.read(rs.getString("angulo_1v1_data"))
          def num(k: String): Int = try j(k).num.toInt catch { case _: Exception => 0 }
          okC += num("central_ok"); gcC += num("central_gc")
          okI += num("izq_ok"); gcI += num("izq_gc")
          okD += num("der_ok"); gcD += num("der_gc")
        } catch { case _: Exception => () }
      }
      val total = okC + gcC + okI + gcI + okD + gcD
      if (total < 15) return Map("suficiente" -> false, "n" -> total)
      def pct(ok: Int, gc: Int): Double = if (ok + gc > 0) ok * 100.0 / (ok + gc) else 0.0
      Map(
        "suficiente" -> true, "n" -> total,
        "central" -> Map("ok" -> okC, "gc" -> gcC, "pct" -> pct(okC, gcC)),
        "izquierda" -> Map("ok" -> okI, "gc" -> gcI, "pct" -> pct(okI, gcI)),
        "derecha" -> Map("ok" -> okD, "gc" -> gcD, "pct" -> pct(okD, gcD))
      )
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE I — DETECCION AUTOMATICA DE SKILLS EN EL FEEDBACK (sin Gemini)
  // ─────────────────────────────────────────────────────────────────────────────
  private val skillFeedbackKeywords: Map[String, List[String]] = Map(
    "POSICION_MANOS"   -> List("manos", "posición de manos"),
    "POSICION_PIES"    -> List("pies", "metatarsos", "posición de set"),
    "CAIDA_DERECHA"    -> List("caída derecha", "lado derecho"),
    "CAIDA_IZQUIERDA"  -> List("caída izquierda", "lado izquierdo"),
    "COMUNICACION"     -> List("comunicación", "voz", "hablar", "organizar"),
    "SALIDAS_AEREAS"   -> List("salidas aéreas", "balones aéreos", "córners"),
    "ANTICIPACION_1V1" -> List("1v1", "uno contra uno", "salida", "achique"),
    "PASE_CORTO"       -> List("pase corto", "salida corta"),
    "PASE_LARGO"       -> List("pase largo", "distribución larga")
  )

  def detectarSkillsEnFeedback(texto: String): List[String] = {
    val lower = texto.toLowerCase
    skillFeedbackKeywords.filter { case (_, kws) => kws.exists(lower.contains) }.keys.toList
  }

  // Mapea el codigo de skill detectado a las palabras clave que se buscan en goalkeeper_skills.habilidad
  private val skillFeedbackHabilidadMatch: Map[String, List[String]] = Map(
    "POSICION_MANOS"   -> List("manos"),
    "POSICION_PIES"    -> List("pies"),
    "CAIDA_DERECHA"    -> List("lateral derecha"),
    "CAIDA_IZQUIERDA"  -> List("lateral izquierda"),
    "COMUNICACION"     -> List("voz", "comunicaci"),
    "SALIDAS_AEREAS"   -> List("aéreo", "aereo"),
    "ANTICIPACION_1V1" -> List("1v1"),
    "PASE_CORTO"       -> List("pase corto"),
    "PASE_LARGO"       -> List("pase largo")
  )

  /** Guarda, para cada skill detectada en el feedback y aun no conseguida, una sugerencia pendiente en feature_cache. */
  def guardarSugerenciasSkillDesdeFeedback(feedback: String): Unit = {
    if (feedback.trim.isEmpty) return
    val detectadas = detectarSkillsEnFeedback(feedback)
    if (detectadas.isEmpty) return
    val conn = getConnection()
    try {
      val rsSkills = conn.createStatement().executeQuery("SELECT id, habilidad FROM goalkeeper_skills WHERE conseguido = FALSE")
      var pendientes = List[(Int, String)]()
      while (rsSkills.next()) pendientes = pendientes :+ (rsSkills.getInt("id"), rsSkills.getString("habilidad"))

      detectadas.foreach { codigo =>
        val claves = skillFeedbackHabilidadMatch.getOrElse(codigo, List.empty)
        pendientes.find { case (_, habilidad) => claves.exists(k => habilidad.toLowerCase.contains(k)) }.foreach {
          case (skillId, habilidad) =>
            val ps = conn.prepareStatement(
              "INSERT INTO feature_cache (cache_key, payload, updated_at) VALUES (?, ?, NOW()) ON CONFLICT (cache_key) DO UPDATE SET payload=EXCLUDED.payload, updated_at=NOW()")
            ps.setString(1, s"skill_sugerida_$skillId")
            ps.setString(2, ujson.write(ujson.Obj("skillId" -> skillId, "habilidad" -> habilidad)))
            ps.executeUpdate()
        }
      }
    } finally { conn.close() }
  }

  def getSugerenciasSkillPendientes(): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT payload FROM feature_cache WHERE cache_key LIKE 'skill_sugerida_%'")
      var l = List[Map[String, Any]]()
      while (rs.next()) {
        try {
          val j = ujson.read(rs.getString("payload"))
          l = l :+ Map("skillId" -> j("skillId").num.toInt, "habilidad" -> fixEncoding(j("habilidad").str))
        } catch { case _: Exception => () }
      }
      l
    } finally { conn.close() }
  }

  def descartarSugerenciaSkill(skillId: Int): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("DELETE FROM feature_cache WHERE cache_key = ?")
      ps.setString(1, s"skill_sugerida_$skillId"); ps.executeUpdate()
    } finally { conn.close() }
  }

  /** Marca la skill como conseguida hoy y limpia la sugerencia pendiente. */
  def confirmarSkillDesdeSugerencia(skillId: Int): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("UPDATE goalkeeper_skills SET conseguido=TRUE, fecha_conseguido=CURRENT_DATE, contexto_conseguido='Detectado en feedback de academia' WHERE id=?")
      ps.setInt(1, skillId); ps.executeUpdate()
    } finally { conn.close() }
    descartarSugerenciaSkill(skillId)
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE J — PERSPECTIVA DEL OJEADOR EXTERNO (amplia /scouting-report)
  // ─────────────────────────────────────────────────────────────────────────────
  /** Solo lectura de cache (30 dias) — NUNCA llama a Gemini. Para usar en el render de la pagina. */
  def getOjeadorExternoCache(): Option[String] = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("SELECT payload FROM feature_cache WHERE cache_key = 'ojeador_externo' AND updated_at > NOW() - INTERVAL '30 days'")
      val rs = ps.executeQuery()
      if (rs.next()) Some(rs.getString("payload")) else None
    } finally { conn.close() }
  }

  /** Llama a Gemini — SOLO desde el boton explicito "Vista del ojeador externo". Cache 30 dias. */
  def generarOjeadorExternoNarrative(): String = {
    val conn = getConnection()
    try {
      val card = getLatestCardData()
      val edad = calcularEdadExacta(card.fechaNacimiento)
      val temporadaId = getTemporadaActivaId()
      val matches = getMatchesList(temporadaId)
      val pj = matches.size
      if (pj < 3) return "Se necesitan al menos 3 partidos registrados esta temporada para generar esta vista."

      val notaMedia = matches.map(_.nota).sum / pj
      def gcOf(m: MatchLog): Int = m.resultado.split("-").lastOption.flatMap(_.trim.toIntOption).getOrElse(0)
      val cleanSheets = matches.count(gcOf(_) == 0)
      val pctCS = cleanSheets * 100 / pj
      val alturaActual = try getBioBandingData().getOrElse("alturaActual", 0.0).asInstanceOf[Double] catch { case _: Exception => 0.0 }

      val datosTemporada = s"Edad: $edad años. Partidos esta temporada: $pj. Nota media: ${"%.1f".format(notaMedia)}. Porterías a cero: $cleanSheets de $pj ($pctCS%%). Altura actual: ${if (alturaActual > 0) f"$alturaActual%.0fcm" else "sin dato"}."

      val prompt = s"""Eres un ojeador profesional que acaba de ver a Héctor por PRIMERA VEZ. No tienes acceso a su historial — solo a los datos de esta temporada. Escribe tu informe en primera persona, tono frío y analítico, como si lo reportaras a tu director deportivo. Incluye: morfología y proyección, nivel técnico observado, perfil mental percibido, proyección de nivel (EN_DESARROLLO / FORMATIVO_MEDIO / ACADEMIA_REGIONAL / ACADEMIA_PRIMERA / ELITE_NACIONAL), recomendación (SEGUIMIENTO_6M / SEGUIMIENTO_12M / NO_SEGUIMIENTO / FICHAR_YA). Máximo 5 párrafos. No menciones Guardian. Datos: $datosTemporada"""

      val texto = AIProvider.ask(prompt, None, bypassCache = true)
      val ps = conn.prepareStatement(
        "INSERT INTO feature_cache (cache_key, payload, updated_at) VALUES ('ojeador_externo', ?, NOW()) ON CONFLICT (cache_key) DO UPDATE SET payload=EXCLUDED.payload, updated_at=NOW()")
      ps.setString(1, texto); ps.executeUpdate()
      texto
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE K — CLUB READINESS SCORE (amplia /market-estimator)
  // ─────────────────────────────────────────────────────────────────────────────
  // SQL/matematicas puras — sin Gemini. Cada factor aporta hasta 20 puntos (total 0-100).
  def getClubReadinessScore(): Map[String, Any] = {
    val conn = getConnection()
    try {
      // 1) Progreso medio del IDP activo (0-20)
      val rsIdp = conn.createStatement().executeQuery("""
        SELECT AVG(o.progreso_pct) as m FROM idp_objetivos o
        JOIN idp_temporadas t ON t.id = o.temporada_id
        WHERE t.estado = 'ACTIVA'
      """)
      val progresoIdp = if (rsIdp.next() && !rsIdp.wasNull()) rsIdp.getDouble("m") / 100.0 * 20.0 else 0.0

      // 2) Rating vs categoria — percentil real RFMF si esta disponible (0-20)
      val ratingCategoria = getPercentilRealHector() match {
        case Some(p) => p("percentilGC").asInstanceOf[Int] / 100.0 * 20.0
        case None => 0.0
      }

      // 3) Estabilidad psicologica — media de motivacion+disfrute del ultimo registro (0-20)
      val rsPsych = conn.createStatement().executeQuery(
        "SELECT motivacion, disfrute FROM psych_records ORDER BY fecha DESC LIMIT 1")
      val estabilidadPsico = if (rsPsych.next()) ((rsPsych.getInt("motivacion") + rsPsych.getInt("disfrute")) / 2.0) / 5.0 * 20.0 else 0.0

      // 4) % de semanas del ultimo mes con ACWR en zona verde (0-20)
      val rsAcwr = conn.createStatement().executeQuery("""
        SELECT COUNT(*) as total, COUNT(CASE WHEN acwr_score >= 8 THEN 1 END) as verde
        FROM forma_diaria WHERE fecha >= CURRENT_DATE - INTERVAL '30 days'
      """)
      val acwrVerdePct = if (rsAcwr.next() && rsAcwr.getInt("total") > 0) rsAcwr.getInt("verde").toDouble / rsAcwr.getInt("total") * 20.0 else 0.0

      // 5) Visibilidad — oportunidades/eventos registrados este año (0-20; 5 eventos = maximo)
      val rsVis = conn.createStatement().executeQuery(
        "SELECT COUNT(*) as n FROM opportunities WHERE EXTRACT(YEAR FROM fecha) = EXTRACT(YEAR FROM CURRENT_DATE)")
      val visibilidad = if (rsVis.next()) math.min(20.0, rsVis.getInt("n") * 4.0) else 0.0

      val readiness = (progresoIdp + ratingCategoria + estabilidadPsico + acwrVerdePct + visibilidad).toInt

      val interpretacion =
        if (readiness <= 30) "No preparado"
        else if (readiness <= 50) "Seguimiento 12m"
        else if (readiness <= 70) "Seguimiento 6m"
        else if (readiness <= 85) "Candidato"
        else "Listo ahora"

      Map(
        "readiness" -> readiness, "interpretacion" -> interpretacion,
        "progresoIdp" -> progresoIdp, "ratingCategoria" -> ratingCategoria,
        "estabilidadPsico" -> estabilidadPsico, "acwrVerdePct" -> acwrVerdePct, "visibilidad" -> visibilidad
      )
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE L — ARCO COMPLETO DE CARRERA (amplia Digital Twin + Markov)
  // ─────────────────────────────────────────────────────────────────────────────
  // SQL/matematicas puras — reusa calcularMarkovPathway(), sin llamadas adicionales a Gemini.
  def getArcoCompletoData(): Map[String, Any] = {
    val card = getLatestCardData()
    val edadActual = calcularEdadExacta(card.fechaNacimiento)
    val categorias = List(("Benjamín", 8), ("Alevín", 10), ("Infantil", 12), ("Cadete", 14), ("Juvenil", 16), ("Profesional", 18))
    val markov = calcularMarkovPathway()

    val proyeccionAcademiaPrimera: Option[Int] = markov.flatMap { m =>
      val estados = List("EN_DESARROLLO", "FORMATIVO_MEDIO", "ACADEMIA_REGIONAL", "ACADEMIA_PRIMERA", "ELITE_NACIONAL")
      val idxActual = m("estadoActualIdx").asInstanceOf[Int]
      val idxAcademiaPrimera = estados.indexOf("ACADEMIA_PRIMERA")
      val velocidad = m("velocidadMejora").asInstanceOf[Double]
      if (idxActual >= idxAcademiaPrimera) Some(edadActual)
      else if (velocidad <= 0) None
      else {
        val pasos = idxAcademiaPrimera - idxActual
        val anios = pasos * (8.0 / velocidad) // mismo supuesto de "gap" de 8 pts FUT por nivel que calcularMarkovPathway()
        Some((edadActual + anios).toInt)
      }
    }

    Map(
      "edadActual" -> edadActual, "categorias" -> categorias,
      "tieneMarkov" -> markov.isDefined, "edadProyectadaAcademiaPrimera" -> proyeccionAcademiaPrimera
    )
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE M — TABLAS CIENTIFICAS PARA TEST FISICOS (normas Eurofit Espana)
  // ─────────────────────────────────────────────────────────────────────────────
  // (p25, p50, p75). Velocidad 10m en segundos (menor es mejor); salto vertical en cm (mayor es mejor).
  private val normasVelocidad10m: Map[Int, (Double, Double, Double)] = Map(
    6 -> (2.80, 2.60, 2.40), 7 -> (2.65, 2.45, 2.25),
    8 -> (2.52, 2.32, 2.12), 9 -> (2.40, 2.20, 2.00),
    10 -> (2.28, 2.08, 1.88)
  )
  private val normasSaltoVertical: Map[Int, (Int, Int, Int)] = Map(
    6 -> (18, 23, 28), 7 -> (21, 27, 33),
    8 -> (24, 30, 37), 9 -> (28, 34, 41),
    10 -> (31, 38, 45)
  )

  /** Percentil e interpretacion de un test fisico frente a las normas Eurofit Espana. None si no hay tabla para ese tipo/edad. */
  def getPercentilTestFisico(tipo: String, valor: Double, edad: Int): Map[String, Any] = {
    def desdeVelocidad(p25: Double, p50: Double, p75: Double): Int =
      if (valor <= p75) 90 else if (valor <= p50) 65 else if (valor <= p25) 35 else 10
    def desdeSalto(p25: Int, p50: Int, p75: Int): Int =
      if (valor >= p75) 90 else if (valor >= p50) 65 else if (valor >= p25) 35 else 10

    val percentilOpt: Option[Int] = tipo match {
      case "velocidad10m" => normasVelocidad10m.get(edad).map { case (p25, p50, p75) => desdeVelocidad(p25, p50, p75) }
      case "saltoVertical" => normasSaltoVertical.get(edad).map { case (p25, p50, p75) => desdeSalto(p25, p50, p75) }
      case _ => None
    }
    percentilOpt match {
      case Some(p) => Map("disponible" -> true, "percentil" -> p, "edad" -> edad, "fuente" -> "Normas Eurofit España")
      case None => Map("disponible" -> false)
    }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE O — RUTINA PRE-PARTIDO
  // ─────────────────────────────────────────────────────────────────────────────
  def saveRutinaDefinicion(descripcion: String): Unit = {
    val conn = getConnection()
    try {
      conn.createStatement().executeUpdate("UPDATE rutina_definicion SET activa = FALSE WHERE activa = TRUE")
      val ps = conn.prepareStatement("INSERT INTO rutina_definicion (descripcion, activa) VALUES (?, TRUE)")
      ps.setString(1, fixEncoding(descripcion)); ps.executeUpdate()
    } finally { conn.close() }
  }

  def getRutinaActiva(): Option[String] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("SELECT descripcion FROM rutina_definicion WHERE activa = TRUE ORDER BY id DESC LIMIT 1")
      if (rs.next()) Some(fixEncoding(rs.getString("descripcion"))) else None
    } finally { conn.close() }
  }

  /** SQL puro — compara la nota media segun si Hector siguio su rutina pre-partido o no. Requiere >=10 partidos con el dato. */
  def getRutinaAnalysis(): Map[String, Any] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT rutina_prepartido, nota FROM matches WHERE status='PLAYED' AND rutina_prepartido IS NOT NULL AND rutina_prepartido != 'SIN_RUTINA'")
      var siList = List[Double](); var noList = List[Double]()
      while (rs.next()) {
        val nota = rs.getDouble("nota")
        rs.getString("rutina_prepartido") match {
          case "SI" => siList = siList :+ nota
          case "NO" => noList = noList :+ nota
          case _ => ()
        }
      }
      val n = siList.size + noList.size
      if (n < 10) return Map("suficiente" -> false, "n" -> n)
      Map(
        "suficiente" -> true, "n" -> n,
        "notaMediaSigue" -> (if (siList.nonEmpty) siList.sum / siList.size else 0.0),
        "notaMediaNoSigue" -> (if (noList.nonEmpty) noList.sum / noList.size else 0.0),
        "nSigue" -> siList.size, "nNoSigue" -> noList.size
      )
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE P — TRANSFERENCIA DE ENTRENAMIENTO
  // ─────────────────────────────────────────────────────────────────────────────
  // SQL puro — conecta el feedback del entrenador de academia con la rubrica del partido siguiente.
  def getTransferenciaEntrenamiento(): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val dimensiones = Seq(
        "rubrica_posicion" -> "Posición", "rubrica_decisiones" -> "Decisiones bajo presión",
        "rubrica_pies" -> "Juego con los pies", "rubrica_comunicacion" -> "Comunicación",
        "rubrica_actitud" -> "Actitud"
      )
      dimensiones.map { case (col, label) =>
        val rs = conn.createStatement().executeQuery(s"""
          SELECT ma.$col as antes, md.$col as despues
          FROM trainings t
          CROSS JOIN LATERAL (
            SELECT $col FROM matches WHERE status='PLAYED' AND fecha::date < t.fecha::date AND $col IS NOT NULL
            ORDER BY fecha DESC LIMIT 1
          ) ma
          CROSS JOIN LATERAL (
            SELECT $col FROM matches WHERE status='PLAYED' AND fecha::date > t.fecha::date
              AND fecha::date <= t.fecha::date + INTERVAL '7 days' AND $col IS NOT NULL
            ORDER BY fecha ASC LIMIT 1
          ) md
          WHERE t.tipo = 'Academia' AND t.feedback_entrenador IS NOT NULL AND t.feedback_entrenador != ''
        """)
        var n = 0; var mejoras = 0
        while (rs.next()) {
          n += 1
          if (rs.getInt("despues") > rs.getInt("antes")) mejoras += 1
        }
        val pct = if (n > 0) mejoras * 100.0 / n else 0.0
        Map("dimension" -> label, "n" -> n, "mejoras" -> mejoras, "pctTransferencia" -> pct)
      }.toList
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE Q — RENDIMIENTO POR FASE DEL PARTIDO
  // ─────────────────────────────────────────────────────────────────────────────
  // El cuarto (Q1-Q4) se deriva del minuto de cada gol, ya registrado en el desglose de goles.
  def saveMinutoGoles(matchId: Int, minutos: List[Int]): Unit = {
    if (matchId <= 0) return
    def cuartoDe(m: Int): String = if (m <= 12) "q1" else if (m <= 25) "q2" else if (m <= 37) "q3" else "q4"
    val counts = scala.collection.mutable.Map("q1" -> 0, "q2" -> 0, "q3" -> 0, "q4" -> 0)
    minutos.foreach(m => counts(cuartoDe(m)) += 1)
    val json = ujson.write(ujson.Obj("q1" -> counts("q1"), "q2" -> counts("q2"), "q3" -> counts("q3"), "q4" -> counts("q4")))
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("UPDATE matches SET minuto_goles = ? WHERE id = ?")
      ps.setString(1, json); ps.setInt(2, matchId); ps.executeUpdate()
    } finally { conn.close() }
  }

  /** Agrega los JSON de minuto_goles y calcula la distribucion de goles por cuarto. Requiere >=15 goles con dato. */
  def getRendimientoPorFase(seasonId: Int = 0): Map[String, Any] = {
    val conn = getConnection()
    try {
      val sf = seasonFilter(seasonId)
      val rs = conn.createStatement().executeQuery(
        s"SELECT minuto_goles FROM matches WHERE status='PLAYED' AND minuto_goles IS NOT NULL AND minuto_goles != '' $sf")
      var q1 = 0; var q2 = 0; var q3 = 0; var q4 = 0
      while (rs.next()) {
        try {
          val j = ujson.read(rs.getString("minuto_goles"))
          def num(k: String): Int = try j(k).num.toInt catch { case _: Exception => 0 }
          q1 += num("q1"); q2 += num("q2"); q3 += num("q3"); q4 += num("q4")
        } catch { case _: Exception => () }
      }
      val total = q1 + q2 + q3 + q4
      if (total < 15) return Map("suficiente" -> false, "n" -> total)
      def pct(n: Int): Double = n * 100.0 / total
      Map(
        "suficiente" -> true, "n" -> total,
        "q1" -> q1, "q2" -> q2, "q3" -> q3, "q4" -> q4,
        "pctQ1" -> pct(q1), "pctQ2" -> pct(q2), "pctQ3" -> pct(q3), "pctQ4" -> pct(q4)
      )
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE R — TESTS DE MOVILIDAD ESPECIFICOS DE PORTERO
  // ─────────────────────────────────────────────────────────────────────────────
  def saveMovilidadTest(fecha: String, alcancePie: Option[Int], rotacionHombro: String,
                         alcanceLateralDer: Option[Int], alcanceLateralIzq: Option[Int], notas: String): Unit = {
    val asimetria = (alcanceLateralDer, alcanceLateralIzq) match {
      case (Some(d), Some(i)) => Some(math.abs(d - i))
      case _ => None
    }
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("""
        INSERT INTO movilidad_tests (fecha, alcance_pie_cm, rotacion_hombro, alcance_lateral_der_cm, alcance_lateral_izq_cm, asimetria_lateral_cm, notas)
        VALUES (?::date, ?, ?, ?, ?, ?, ?)
      """)
      ps.setString(1, if (fecha.nonEmpty) fecha else LocalDate.now().toString)
      def setOptInt(idx: Int, v: Option[Int]): Unit = v match { case Some(x) => ps.setInt(idx, x); case None => ps.setNull(idx, java.sql.Types.INTEGER) }
      setOptInt(2, alcancePie)
      if (rotacionHombro.nonEmpty) ps.setString(3, rotacionHombro) else ps.setNull(3, java.sql.Types.VARCHAR)
      setOptInt(4, alcanceLateralDer); setOptInt(5, alcanceLateralIzq); setOptInt(6, asimetria)
      ps.setString(7, fixEncoding(notas))
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def getMovilidadTests(): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("SELECT * FROM movilidad_tests ORDER BY fecha ASC")
      var l = List[Map[String, Any]]()
      while (rs.next()) {
        def optInt(col: String): Option[Int] = { val v = rs.getInt(col); if (rs.wasNull()) None else Some(v) }
        l = l :+ Map(
          "id" -> rs.getInt("id"), "fecha" -> rs.getDate("fecha").toString,
          "alcancePie" -> optInt("alcance_pie_cm"), "rotacionHombro" -> Option(rs.getString("rotacion_hombro")).getOrElse(""),
          "alcanceLateralDer" -> optInt("alcance_lateral_der_cm"), "alcanceLateralIzq" -> optInt("alcance_lateral_izq_cm"),
          "asimetria" -> optInt("asimetria_lateral_cm"), "notas" -> fixEncoding(Option(rs.getString("notas")).getOrElse(""))
        )
      }
      l
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE S — TOOLKIT DE REGULACION EMOCIONAL
  // ─────────────────────────────────────────────────────────────────────────────
  // SQL puro — distribucion de comportamientos tras gol encajado + correlacion con la nota. Requiere >=8 registros.
  def getRegulacionEmocional(): Map[String, Any] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT regulacion_emocional, nota FROM matches WHERE status='PLAYED' AND goles_contra > 0 AND regulacion_emocional IS NOT NULL")
      var n = 0
      val conteo = scala.collection.mutable.Map[String, Int]().withDefaultValue(0)
      val notasPorComportamiento = scala.collection.mutable.Map[String, List[Double]]().withDefaultValue(Nil)
      while (rs.next()) {
        n += 1
        val comp = rs.getString("regulacion_emocional")
        conteo(comp) += 1
        notasPorComportamiento(comp) = notasPorComportamiento(comp) :+ rs.getDouble("nota")
      }
      if (n < 8) return Map("suficiente" -> false, "n" -> n)

      val distribucion = conteo.toList.map { case (comp, c) => Map("comportamiento" -> comp, "n" -> c, "pct" -> (c * 100.0 / n)) }
      val notaReorganiza = notasPorComportamiento.get("REORGANIZA").filter(_.nonEmpty).map(l => l.sum / l.size)
      val notaDecaido = notasPorComportamiento.get("DECAIDO").filter(_.nonEmpty).map(l => l.sum / l.size)
      val patronDominante = conteo.toList.sortBy(-_._2).headOption.map(_._1).getOrElse("")
      val pctDecaido = conteo("DECAIDO") * 100.0 / n
      val pctSaludable = (conteo("REORGANIZA") + conteo("RESPIRA")) * 100.0 / n

      Map(
        "suficiente" -> true, "n" -> n, "distribucion" -> distribucion,
        "notaReorganiza" -> notaReorganiza, "notaDecaido" -> notaDecaido,
        "patronDominante" -> patronDominante, "pctDecaido" -> pctDecaido, "pctSaludable" -> pctSaludable
      )
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // MODULO — ARQUETIPO DE PORTERO
  // ─────────────────────────────────────────────────────────────────────────────
  // SQL puro — sin Gemini. Lee de tablas ya existentes (matches, rubrica, etc).
  def calcularArquetipoPortero(seasonId: Int = 0): Map[String, Any] = {
    val conn = getConnection()
    try {
      val sid = if (seasonId > 0) seasonId else getTemporadaActivaId()
      if (sid == 0) return Map("activo" -> false, "motivo" -> "Sin temporada activa")

      // ── Recoger metricas base ─────────────────────────────────────────
      val rsScan = conn.createStatement().executeQuery(s"""
        SELECT AVG(CASE WHEN scanning_rate > 0
          THEN scanning_efectivo::float / scanning_rate ELSE NULL END) as scan_eff,
          AVG(scanning_rate) as scan_freq
        FROM matches WHERE season_id=$sid AND status='PLAYED'""")
      val (scanEff, scanFreq) = if (rsScan.next())
        (rsScan.getDouble("scan_eff"), rsScan.getDouble("scan_freq")) else (0.0, 0.0)

      val rsBp = conn.createStatement().executeQuery(s"""
        SELECT AVG(lineas_superadas) as bypass
        FROM matches WHERE season_id=$sid AND status='PLAYED'""")
      val bypass = if (rsBp.next()) rsBp.getDouble("bypass") else 0.0

      val rs1v1 = conn.createStatement().executeQuery(s"""
        SELECT AVG(paradas_1v1) as par1v1,
          AVG(paradas_aereas) as aereas,
          AVG(acciones_pie) as pie,
          AVG(COALESCE(rubrica_comunicacion, 3)) as com,
          AVG(COALESCE(rubrica_posicion, 3)) as pos,
          AVG(COALESCE(rubrica_decisiones, 3)) as dec,
          AVG(COALESCE(rubrica_pies, 3)) as pies,
          AVG(COALESCE(rubrica_actitud, 3)) as act,
          AVG(COALESCE(economia_movimiento, 3)) as economia,
          AVG(COALESCE(corners_dominados::float /
            NULLIF(corners_dominados + corners_cedidos, 0), 0)) as dominio_aereo,
          AVG(CASE WHEN velocidad_distribucion = 'INMEDIATO' THEN 1.0
              WHEN velocidad_distribucion = 'NORMAL' THEN 0.6
              WHEN velocidad_distribucion = 'LENTO' THEN 0.2
              ELSE 0.5 END) as vel_dist,
          COUNT(*) as pj
        FROM matches WHERE season_id=$sid AND status='PLAYED'""")

      if (!rs1v1.next() || rs1v1.getInt("pj") < 5)
        return Map("activo" -> false, "motivo" -> "Mínimo 5 partidos necesarios")

      val (par1v1, aereas, pie, com, pos, dec, pies, act, economia, dominioAereo, velDist, pj) = (
        rs1v1.getDouble("par1v1"), rs1v1.getDouble("aereas"),
        rs1v1.getDouble("pie"), rs1v1.getDouble("com"),
        rs1v1.getDouble("pos"), rs1v1.getDouble("dec"),
        rs1v1.getDouble("pies"), rs1v1.getDouble("act"),
        rs1v1.getDouble("economia"), rs1v1.getDouble("dominio_aereo"),
        rs1v1.getDouble("vel_dist"), rs1v1.getInt("pj")
      )

      // ── Normalizar cada metrica a escala 0-100 ────────────────────────
      def norm(v: Double, min: Double, max: Double): Double =
        Math.min(100, Math.max(0, (v - min) / (max - min) * 100))

      val nScanEff    = norm(scanEff, 0.0, 1.0)
      val nScanFreq   = norm(scanFreq, 0.0, 10.0)
      val nBypass     = norm(bypass, 0.0, 5.0)
      val n1v1        = norm(par1v1, 0.0, 5.0)
      val nAereas     = norm(aereas, 0.0, 5.0)
      val nPie        = norm(pie, 0.0, 8.0)
      val nCom        = norm(com, 1.0, 5.0)
      val nPos        = norm(pos, 1.0, 5.0)
      val nDec        = norm(dec, 1.0, 5.0)
      val nPies       = norm(pies, 1.0, 5.0)
      val nAct        = norm(act, 1.0, 5.0)
      val nEconomia   = norm(economia, 1.0, 5.0)
      val nDominioAer = norm(dominioAereo, 0.0, 1.0)
      val nVelDist    = norm(velDist, 0.0, 1.0)

      // ── Puntuacion por arquetipo (0-100) ──────────────────────────────
      val sweeper = (
        nScanEff    * 0.25 +
        nScanFreq   * 0.15 +
        nBypass     * 0.25 +
        n1v1        * 0.20 +
        nDec        * 0.15
      ).toInt

      val shotStopper = (
        nEconomia   * 0.35 +
        nPos        * 0.35 +
        nAereas     * 0.15 +
        nAct        * 0.15
      ).toInt

      val commanding = (
        nDominioAer * 0.35 +
        nCom        * 0.35 +
        nAereas     * 0.15 +
        nAct        * 0.15
      ).toInt

      val modern = (
        nVelDist    * 0.25 +
        nPies       * 0.25 +
        nPie        * 0.20 +
        ((nPos + nDec + nCom + nPies + nAct) / 5.0) * 0.30
      ).toInt

      val arquetipos = List(
        ("SWEEPER_KEEPER", sweeper, "🔵"),
        ("SHOT_STOPPER", shotStopper, "🔴"),
        ("COMMANDING_KEEPER", commanding, "🟡"),
        ("MODERN_GUARDIAN", modern, "🟢")
      ).sortBy(-_._2)

      val dominante = arquetipos.head
      val secundario = arquetipos(1)

      // Guardar en arquetipo_history si han pasado mas de 30 dias
      val rsLast = conn.createStatement().executeQuery(
        s"SELECT fecha_calculo FROM arquetipo_history WHERE season_id=$sid ORDER BY fecha_calculo DESC LIMIT 1")
      val debeGuardar = !rsLast.next() ||
        rsLast.getDate("fecha_calculo").toLocalDate.isBefore(LocalDate.now().minusDays(30))
      if (debeGuardar) {
        val ps = conn.prepareStatement("""
          INSERT INTO arquetipo_history
            (season_id, pct_sweeper, pct_shot_stopper, pct_commanding, pct_modern,
             arquetipo_dominante, arquetipo_secundario, partidos_base)
          VALUES (?,?,?,?,?,?,?,?)""")
        ps.setInt(1, sid); ps.setInt(2, sweeper); ps.setInt(3, shotStopper)
        ps.setInt(4, commanding); ps.setInt(5, modern)
        ps.setString(6, dominante._1); ps.setString(7, secundario._1)
        ps.setInt(8, pj)
        ps.executeUpdate()
      }

      Map(
        "activo"           -> true,
        "sweeper"          -> sweeper,
        "shotStopper"      -> shotStopper,
        "commanding"       -> commanding,
        "modern"           -> modern,
        "dominante"        -> dominante._1,
        "dominanteLabel"   -> dominante._3,
        "dominantePct"     -> dominante._2,
        "secundario"       -> secundario._1,
        "secundarioPct"    -> secundario._2,
        "pj"               -> pj
      )
    } finally { conn.close() }
  }

  def getArquetipoHistory(seasonId: Int = 0): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val sid = if (seasonId > 0) seasonId else getTemporadaActivaId()
      val ps = conn.prepareStatement("SELECT * FROM arquetipo_history WHERE season_id = ? ORDER BY fecha_calculo ASC")
      ps.setInt(1, sid)
      val rs = ps.executeQuery()
      var l = List[Map[String, Any]]()
      while (rs.next()) l = l :+ Map(
        "fecha" -> rs.getDate("fecha_calculo").toString,
        "sweeper" -> rs.getInt("pct_sweeper"), "shotStopper" -> rs.getInt("pct_shot_stopper"),
        "commanding" -> rs.getInt("pct_commanding"), "modern" -> rs.getInt("pct_modern")
      )
      l
    } finally { conn.close() }
  }

  def arquetipoDescripcion(tipo: String): Map[String, String] = tipo match {
    case "SWEEPER_KEEPER" => Map(
      "nombre"       -> "Sweeper-Keeper",
      "emoji"        -> "🔵",
      "referentes"   -> "Neuer, Ederson, Alisson",
      "descripcion"  -> "Sale del área con decisión, actúa como líbero adicional, alto scanning y distribución. Domina el 1v1 agresivo.",
      "entreno_foco" -> "Salidas agresivas, pase largo con presión, anticipación visual, lectura del juego anticipada.",
      "sistema_ideal"-> "Presión alta, salida de balón desde atrás, portero-jugador de campo.",
      "alerta"       -> "Tendencia a salir tarde o a arriesgarse en exceso. Trabajar el juicio de cuándo salir y cuándo quedarse."
    )
    case "SHOT_STOPPER" => Map(
      "nombre"       -> "Shot-Stopper",
      "emoji"        -> "🔴",
      "referentes"   -> "Oblak, Courtois, Ter Stegen (primera época)",
      "descripcion"  -> "Reflejos excepcionales, posición de set perfecta, mínimo movimiento máxima cobertura. Domina desde su posición.",
      "entreno_foco" -> "Posición de set, reacción ante disparo, posicionamiento en tiros lejanos, economía de movimiento.",
      "sistema_ideal"-> "Bloque medio-bajo, equipos que defienden con orden y necesitan un portero que salve lo que la defensa no corta.",
      "alerta"       -> "Puede ser pasivo en el juego con el pie y en la salida al área. Trabajar distribución y lectura anticipada."
    )
    case "COMMANDING_KEEPER" => Map(
      "nombre"       -> "Commanding Keeper",
      "emoji"        -> "🟡",
      "referentes"   -> "Casillas, Buffon, Valdés",
      "descripcion"  -> "Dominio aéreo absoluto, alta comunicación y liderazgo vocal. Organiza la defensa y manda en el área.",
      "entreno_foco" -> "Mando en área, comunicación táctica, salidas a balones aéreos, liderazgo bajo presión.",
      "sistema_ideal"-> "Equipos que reciben mucho balón aéreo, con defensas jóvenes que necesitan organización constante.",
      "alerta"       -> "Puede descuidar el juego con el pie y la distribución rápida. Trabajar la transición parada-distribución."
    )
    case "MODERN_GUARDIAN" => Map(
      "nombre"       -> "Modern Guardian",
      "emoji"        -> "🟢",
      "referentes"   -> "ter Stegen, Alisson, Donnarumma",
      "descripcion"  -> "Equilibrado en todas las dimensiones. Distribución precisa, transición parada-juego rápida, sólido en todo.",
      "entreno_foco" -> "Desarrollo balanceado. No especializar prematuramente — mantener el equilibrio entre todas las dimensiones.",
      "sistema_ideal"-> "Cualquier sistema. La versatilidad es su mayor activo.",
      "alerta"       -> "Sin especialización puede perderse en academias que buscan un perfil muy definido. Identificar y potenciar el punto diferencial."
    )
    case _ => Map("nombre" -> tipo, "emoji" -> "⚪", "referentes" -> "", "descripcion" -> "", "entreno_foco" -> "", "sistema_ideal" -> "", "alerta" -> "")
  }

  /** Solo lectura de cache (30 dias) — NUNCA llama a Gemini. */
  def getArquetipoAnalisisCache(): Option[String] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT payload FROM feature_cache WHERE cache_key = 'arquetipo_analisis_ia' AND updated_at > NOW() - INTERVAL '30 days'")
      if (rs.next()) Some(rs.getString("payload")) else None
    } finally { conn.close() }
  }

  /** Llama a Gemini — SOLO desde el boton explicito "Análisis IA del arquetipo". Cache 30 dias. */
  def generarArquetipoAnalisisIA(): String = {
    val conn = getConnection()
    try {
      val arq = calcularArquetipoPortero()
      if (!arq("activo").asInstanceOf[Boolean]) return "Se necesitan al menos 5 partidos registrados para generar este análisis."

      val card = getLatestCardData()
      val edad = calcularEdadExacta(card.fechaNacimiento)
      val pj = arq("pj").asInstanceOf[Int]
      val ordenados = List(
        ("Sweeper-Keeper", arq("sweeper").asInstanceOf[Int]),
        ("Shot-Stopper", arq("shotStopper").asInstanceOf[Int]),
        ("Commanding Keeper", arq("commanding").asInstanceOf[Int]),
        ("Modern Guardian", arq("modern").asInstanceOf[Int])
      ).sortBy(-_._2)
      val dominanteNombre = arquetipoDescripcion(arq("dominante").asInstanceOf[String])("nombre")
      val perfilStr = ordenados.map { case (n, p) => s"$n $p%" }.mkString(" · ")
      val metricasContribuyen = arquetipoDescripcion(arq("dominante").asInstanceOf[String])("entreno_foco")

      val prompt = s"""Eres el director de metodología de porteros de una academia de élite. Héctor es un portero de $edad años con $pj partidos registrados. Su perfil de arquetipo calculado es: $perfilStr. Las métricas que más contribuyen a su arquetipo dominante ($dominanteNombre) son: $metricasContribuyen. Analiza: 1) Si el arquetipo emergente es coherente con su perfil psicológico y físico actual, 2) Si hay tensión entre su arquetipo dominante y secundario o si son complementarios, 3) Qué debería priorizar el entrenador de academia en los próximos 3 meses para potenciar su arquetipo natural. Máximo 3 líneas por punto. Tono técnico, orientado al entrenador."""

      val texto = AIProvider.ask(prompt, None, bypassCache = true)
      val ps = conn.prepareStatement(
        "INSERT INTO feature_cache (cache_key, payload, updated_at) VALUES ('arquetipo_analisis_ia', ?, NOW()) ON CONFLICT (cache_key) DO UPDATE SET payload=EXCLUDED.payload, updated_at=NOW()")
      ps.setString(1, texto); ps.executeUpdate()
      texto
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // MODULO — LA VOZ DEL PORTERO
  // ─────────────────────────────────────────────────────────────────────────────
  def caritaEmoji(v: Int): String = v match {
    case 5 => "😄"; case 4 => "😊"; case 3 => "😐"; case 2 => "😟"; case 1 => "😢"; case _ => "❔"
  }

  private def vozPorteroRowToMap(rs: java.sql.ResultSet): Map[String, Any] = Map(
    "id" -> rs.getInt("id"), "fecha" -> rs.getDate("fecha").toString,
    "motivacionCarita" -> rs.getInt("motivacion_carita"),
    "respuestaError" -> fixEncoding(rs.getString("respuesta_error")),
    "respuestaAprendizaje" -> fixEncoding(rs.getString("respuesta_aprendizaje")),
    "analisisIA" -> Option(rs.getString("analisis_ia")).map(fixEncoding),
    "analisisFecha" -> Option(rs.getTimestamp("analisis_fecha")).map(_.toString).getOrElse("")
  )

  /** Upsert: un unico registro por mes natural. Si ya existia, lo sobreescribe y limpia el analisis (queda obsoleto). */
  def saveVozPortero(motivacion: Int, respuestaError: String, respuestaAprendizaje: String): Int = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("""
        INSERT INTO voz_portero (fecha, motivacion_carita, respuesta_error, respuesta_aprendizaje)
        VALUES (CURRENT_DATE, ?, ?, ?)
        ON CONFLICT (DATE_TRUNC('month', fecha::timestamp)) DO UPDATE SET
          motivacion_carita = EXCLUDED.motivacion_carita,
          respuesta_error = EXCLUDED.respuesta_error,
          respuesta_aprendizaje = EXCLUDED.respuesta_aprendizaje,
          analisis_ia = NULL, analisis_fecha = NULL
        RETURNING id
      """)
      ps.setInt(1, motivacion)
      ps.setString(2, fixEncoding(respuestaError))
      ps.setString(3, fixEncoding(respuestaAprendizaje))
      val rs = ps.executeQuery()
      if (rs.next()) rs.getInt("id") else -1
    } finally { conn.close() }
  }

  /** Llama a Gemini — SOLO en background tras guardar. Nunca en el render de /voz-portero. */
  def analizarVozPortero(id: Int): Unit = {
    new Thread(() => {
      var conn: Connection = null
      try {
        conn = getConnection()
        val ps = conn.prepareStatement("SELECT motivacion_carita, respuesta_error, respuesta_aprendizaje FROM voz_portero WHERE id = ?")
        ps.setInt(1, id)
        val rs = ps.executeQuery()
        if (rs.next()) {
          val card = getLatestCardData(); val edad = calcularEdadExacta(card.fechaNacimiento)
          val motivacion = rs.getInt("motivacion_carita")
          val error = fixEncoding(rs.getString("respuesta_error"))
          val aprendizaje = fixEncoding(rs.getString("respuesta_aprendizaje"))

          val prompt = s"""Eres un psicólogo deportivo especializado en desarrollo infantil y rendimiento deportivo. Héctor es un portero de $edad años. Este mes ha respondido lo siguiente con sus propias palabras: MOTIVACIÓN: $motivacion/5. ANTE UN ERROR: '$error'. QUÉ MÁS LE GUSTA APRENDER: '$aprendizaje'. Analiza en tres párrafos cortos: 1) MOTIVACIÓN INTRÍNSECA: ¿Qué revela la carita y la respuesta sobre su motivación real? ¿Es disfrute genuino, motivación extrínseca o hay señales de ambivalencia? 2) RELACIÓN CON LOS ERRORES: ¿Qué revela su respuesta sobre cómo procesa el fallo? ¿Resiliencia, perfeccionismo, indiferencia? ¿Qué implica esto para cómo el padre debe gestionar los errores con él? 3) ORIENTACIÓN AL APRENDIZAJE: ¿Qué dice su respuesta sobre qué tipo de portero quiere ser? ¿Hay alguna pista sobre su arquetipo emergente? Un párrafo final: CONSEJO DEL MES: una sola acción concreta que el padre puede hacer esta semana basada en lo que Héctor dijo — sin hablar de fútbol, sin correcciones técnicas. Que sea algo del ámbito familiar. Lenguaje humano, cálido, no clínico."""

          val analisis = AIProvider.ask(prompt, None, bypassCache = true)
          val up = conn.prepareStatement("UPDATE voz_portero SET analisis_ia = ?, analisis_fecha = NOW() WHERE id = ?")
          up.setString(1, analisis); up.setInt(2, id)
          up.executeUpdate()
        }
      } catch { case e: Exception => println(s"[!] analizarVozPortero error: ${e.getMessage}") }
      finally { if (conn != null) conn.close() }
    }).start()
  }

  def getVozPorteroMesActual(): Option[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT * FROM voz_portero WHERE DATE_TRUNC('month', fecha) = DATE_TRUNC('month', CURRENT_DATE) ORDER BY id DESC LIMIT 1")
      if (rs.next()) Some(vozPorteroRowToMap(rs)) else None
    } finally { conn.close() }
  }

  def getVozPorteroHistorial(): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("SELECT * FROM voz_portero ORDER BY fecha DESC")
      var l = List[Map[String, Any]]()
      while (rs.next()) l = l :+ vozPorteroRowToMap(rs)
      l
    } finally { conn.close() }
  }

  // BLOQUE L: busqueda libre en las respuestas de La Voz del Portero
  def searchVozPortero(query: String): List[Map[String, Any]] = {
    val q = query.trim
    if (q.isEmpty) return Nil
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("""
        SELECT id, fecha, motivacion_carita, respuesta_error, respuesta_aprendizaje
        FROM voz_portero
        WHERE respuesta_error ILIKE '%' || ? || '%'
           OR respuesta_aprendizaje ILIKE '%' || ? || '%'
        ORDER BY fecha DESC""")
      // % y _ son comodines de LIKE: se escapan para buscar el texto literal
      val literal = q.replace("\\", "\\\\").replace("%", "\\%").replace("_", "\\_")
      ps.setString(1, literal); ps.setString(2, literal)
      val rs = ps.executeQuery()
      Iterator.continually(rs).takeWhile(_.next()).map { r =>
        Map[String, Any](
          "id" -> r.getInt("id"), "fecha" -> r.getDate("fecha").toString,
          "motivacionCarita" -> r.getInt("motivacion_carita"),
          "respuestaError" -> fixEncoding(Option(r.getString("respuesta_error")).getOrElse("")),
          "respuestaAprendizaje" -> fixEncoding(Option(r.getString("respuesta_aprendizaje")).getOrElse("")))
      }.toList
    } finally { conn.close() }
  }

  /** Meses consecutivos (contando desde el mas reciente) con carita <= 3. None si la racha es menor de 3. */
  def getAlertaMotivacionVoz(): Option[String] = {
    val hist = getVozPorteroHistorial() // ya viene ordenado por fecha DESC
    var n = 0
    var mesEsperado: Option[java.time.YearMonth] = None
    var continuar = true
    hist.foreach { h =>
      if (continuar) {
        val fecha = java.time.LocalDate.parse(h("fecha").asInstanceOf[String])
        val ym = java.time.YearMonth.from(fecha)
        val carita = h("motivacionCarita").asInstanceOf[Int]
        mesEsperado match {
          case Some(esperado) if ym != esperado => continuar = false
          case _ =>
            if (carita <= 3) { n += 1; mesEsperado = Some(ym.minusMonths(1)) } else continuar = false
        }
      }
    }
    if (n >= 3) Some(s"⚠️ La motivación de Héctor lleva $n meses por debajo de la mitad. Revisa el registro psicológico y habla con el entrenador de academia.")
    else None
  }

  def getDiasDesdeUltimaVoz(): Option[Long] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("SELECT MAX(fecha) as f FROM voz_portero")
      if (rs.next() && rs.getDate("f") != null) Some(java.time.temporal.ChronoUnit.DAYS.between(rs.getDate("f").toLocalDate, LocalDate.now()))
      else None
    } finally { conn.close() }
  }

  /** True si toca recordar el registro mensual (nunca registrado, o hace mas de 35 dias). */
  def debeRecordarVozPortero(): Boolean = getDiasDesdeUltimaVoz().forall(_ > 35)

  /** Texto listo para incluir como contexto en getDeepAnalysis(). None si no hay registros recientes (~3 meses). */
  def getVozPorteroTendenciaTexto(): Option[String] = {
    val hist = getVozPorteroHistorial()
    if (hist.isEmpty) return None
    val ultimaFecha = java.time.LocalDate.parse(hist.head("fecha").asInstanceOf[String])
    if (java.time.temporal.ChronoUnit.DAYS.between(ultimaFecha, LocalDate.now()) > 100) return None
    val ultimoValor = hist.head("motivacionCarita").asInstanceOf[Int]
    val anteriores = hist.drop(1).take(3).map(_("motivacionCarita").asInstanceOf[Int])
    if (anteriores.isEmpty) return Some(s"Motivación declarada por Héctor (sus propias palabras): $ultimoValor/5 este mes.")
    val mediaAnterior = anteriores.sum.toDouble / anteriores.size
    val flecha = if (ultimoValor > mediaAnterior + 0.3) "↑" else if (ultimoValor < mediaAnterior - 0.3) "↓" else "→"
    Some(s"Motivación declarada por Héctor (sus propias palabras): $ultimoValor/5 este mes, tendencia $flecha respecto al trimestre anterior.")
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // FIX 2 — ACWR CON ESTADO (evita el 4.00 por historico insuficiente)
  // ─────────────────────────────────────────────────────────────────────────────
  // SQL puro. Para UI que debe distinguir "sin datos suficientes" de un ACWR real.
  def calcularACWRConEstado(): Map[String, Any] = {
    val conn = getConnection()
    try {
      // Semanas (de las ultimas 4) con al menos una sesion registrada (partido o entreno)
      val rsSemanas = conn.createStatement().executeQuery("""
        SELECT COUNT(DISTINCT TO_CHAR(fecha, 'IYYY-IW')) as semanas
        FROM (
          (SELECT fecha FROM matches WHERE status='PLAYED' AND fecha >= CURRENT_DATE - 28)
          UNION ALL
          (SELECT fecha FROM trainings WHERE fecha >= CURRENT_DATE - 28)
        ) t
      """)
      val semanasConDatos = if (rsSemanas.next()) rsSemanas.getInt("semanas") else 0

      val acute = getWorkloads(7)
      val chronic = getWorkloads(28)
      val cargaAguda = if (acute.nonEmpty) acute.sum / 7.0 else 0.0
      val cargaCronica = if (chronic.nonEmpty) chronic.sum / 28.0 else 0.0

      if (semanasConDatos < 3) {
        Map(
          "acwr"    -> 0.0,
          "status"  -> "INSUFICIENTE",
          "mensaje" -> "Se necesitan al menos 3 semanas de datos para calcular el ACWR con precisión",
          "aguda"   -> cargaAguda,
          "cronica" -> cargaCronica,
          "semanasConDatos" -> semanasConDatos
        )
      } else {
        // Floor minimo para evitar divisiones por casi cero
        val cargaCronicaMin = Math.max(cargaCronica, cargaAguda * 0.3)
        val acwr = if (cargaCronicaMin > 0) cargaAguda / cargaCronicaMin else 0.0
        Map(
          "acwr" -> acwr, "status" -> "OK", "mensaje" -> "",
          "aguda" -> cargaAguda, "cronica" -> cargaCronica, "semanasConDatos" -> semanasConDatos
        )
      }
    } finally { conn.close() }
  }

  // BLOQUE B (autoeval padre): cruza conducta_padre con la nota del partido. NUNCA usar en
  // /hector ni en el informe de captacion — es un dato privado del padre, no del jugador.
  def getConductaPadreAnalysis(): Map[String, Any] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT conducta_padre, nota FROM matches WHERE status='PLAYED' AND conducta_padre IS NOT NULL")
      var n = 0; var sumaConducta = 0.0
      var puntos = List[(Int, Double)]()
      while (rs.next()) {
        n += 1
        val c = rs.getInt("conducta_padre")
        sumaConducta += c
        puntos = puntos :+ (c, rs.getDouble("nota"))
      }
      if (n < 10) return Map("suficiente" -> false, "n" -> n)
      val mediaConducta = sumaConducta / n
      val notaMediaIntervencionista = puntos.filter(_._1 <= 2).map(_._2) match { case l if l.nonEmpty => l.sum / l.size; case _ => 0.0 }
      val notaMediaObservador = puntos.filter(_._1 >= 4).map(_._2) match { case l if l.nonEmpty => l.sum / l.size; case _ => 0.0 }
      Map(
        "suficiente" -> true, "n" -> n, "mediaConducta" -> mediaConducta,
        "notaMediaIntervencionista" -> notaMediaIntervencionista, "notaMediaObservador" -> notaMediaObservador
      )
    } finally { conn.close() }
  }

  // BLOQUE B1: guarda temporalmente la autopercepcion de Hector antes de que el partido
  // se registre formalmente — se lee desde el formulario de partido para pre-rellenar.
  def guardarAutopercepcionTemporal(valor: Int): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement(
        "INSERT INTO feature_cache (cache_key, payload, updated_at) VALUES (?, ?, NOW()) ON CONFLICT (cache_key) DO UPDATE SET payload=EXCLUDED.payload, updated_at=NOW()")
      ps.setString(1, s"autopercepcion_prepartido_${LocalDate.now().toString}")
      ps.setString(2, valor.toString)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def getAutopercepcionTemporalHoy(): Option[Int] = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("SELECT payload FROM feature_cache WHERE cache_key = ?")
      ps.setString(1, s"autopercepcion_prepartido_${LocalDate.now().toString}")
      val rs = ps.executeQuery()
      if (rs.next()) rs.getString("payload").toIntOption else None
    } finally { conn.close() }
  }

  // BLOQUE B3: cruza la autopercepcion pre-partido con el indice de forma calculado ese dia
  def getAutopercepcionVsForma(): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("""
        SELECT m.fecha, m.rival, m.autopercepcion_prepartido, m.nota, f.indice_forma
        FROM matches m
        JOIN forma_diaria f ON f.fecha = m.fecha::date
        WHERE m.status='PLAYED' AND m.autopercepcion_prepartido IS NOT NULL AND f.indice_forma IS NOT NULL
        ORDER BY m.fecha DESC
      """)
      var l = List[Map[String, Any]]()
      while (rs.next()) {
        val autop = rs.getInt("autopercepcion_prepartido")
        val forma = rs.getDouble("indice_forma")
        // Normaliza el indice de forma (0-10) a escala 1-5 para poder comparar con la autopercepcion
        val formaEn5 = math.min(5.0, math.max(1.0, forma / 2.0))
        l = l :+ Map(
          "fecha" -> rs.getDate("fecha").toString, "rival" -> Option(rs.getString("rival")).getOrElse(""),
          "autopercepcion" -> autop, "indiceForma" -> forma, "formaEn5" -> formaEn5,
          "divergencia" -> (autop - formaEn5), "nota" -> rs.getDouble("nota")
        )
      }
      l
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE D — DESGLOSE TECNICO DE PARADAS
  // ─────────────────────────────────────────────────────────────────────────────
  def saveParadaDetalle(matchId: Int, numeroParada: Int, tecnica: String, parteCuerpo: String, resultado: String, zonaOrigen: String): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement(
        "INSERT INTO paradas_detalle (match_id, numero_parada, tecnica, parte_cuerpo, resultado, zona_origen) VALUES (?,?,?,?,?,?)")
      ps.setInt(1, matchId); ps.setInt(2, numeroParada)
      if (tecnica.nonEmpty) ps.setString(3, tecnica) else ps.setNull(3, java.sql.Types.VARCHAR)
      if (parteCuerpo.nonEmpty) ps.setString(4, parteCuerpo) else ps.setNull(4, java.sql.Types.VARCHAR)
      if (resultado.nonEmpty) ps.setString(5, resultado) else ps.setNull(5, java.sql.Types.VARCHAR)
      if (zonaOrigen.nonEmpty) ps.setString(6, zonaOrigen) else ps.setNull(6, java.sql.Types.VARCHAR)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def deleteParadasDetalle(matchId: Int): Unit = {
    val conn = getConnection()
    try { val ps = conn.prepareStatement("DELETE FROM paradas_detalle WHERE match_id = ?"); ps.setInt(1, matchId); ps.executeUpdate() }
    finally { conn.close() }
  }

  def getParadasDetalleMatch(matchId: Int): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("SELECT numero_parada, tecnica, parte_cuerpo, resultado, zona_origen FROM paradas_detalle WHERE match_id=? ORDER BY numero_parada ASC")
      ps.setInt(1, matchId)
      val rs = ps.executeQuery()
      var l = List[Map[String, Any]]()
      while (rs.next()) l = l :+ Map(
        "numeroParada" -> rs.getInt("numero_parada"),
        "tecnica" -> Option(rs.getString("tecnica")).getOrElse(""),
        "parteCuerpo" -> Option(rs.getString("parte_cuerpo")).getOrElse(""),
        "resultado" -> Option(rs.getString("resultado")).getOrElse(""),
        "zonaOrigen" -> Option(rs.getString("zona_origen")).getOrElse("")
      )
      l
    } finally { conn.close() }
  }

  // SQL puro — agrupa por tecnica, calcula la mas usada y la de mejor tasa de resultado limpio
  def getParadasAnalysis(seasonId: Int = 0): Map[String, Any] = {
    val conn = getConnection()
    try {
      val sf = seasonFilter(seasonId, "m")
      val rs = conn.createStatement().executeQuery(s"""
        SELECT pd.tecnica, COUNT(*) as usos,
               COUNT(CASE WHEN pd.resultado IN ('ATRAPADO_LIMPIO','DESPEJADO_ZONA_SEGURA') THEN 1 END) as limpias
        FROM paradas_detalle pd
        JOIN matches m ON m.id = pd.match_id
        WHERE pd.tecnica IS NOT NULL $sf
        GROUP BY pd.tecnica
        ORDER BY usos DESC
      """)
      var porTecnica = List[Map[String, Any]]()
      var total = 0
      while (rs.next()) {
        val usos = rs.getInt("usos"); total += usos
        val limpias = rs.getInt("limpias")
        val pct = if (usos > 0) limpias * 100.0 / usos else 0.0
        porTecnica = porTecnica :+ Map("tecnica" -> rs.getString("tecnica"), "usos" -> usos, "limpias" -> limpias, "pctLimpio" -> pct)
      }
      val masUsada = porTecnica.sortBy(m => -m("usos").asInstanceOf[Int]).headOption.map(_("tecnica").asInstanceOf[String])
      val mejorTasa = porTecnica.filter(_("usos").asInstanceOf[Int] >= 3).sortBy(m => -m("pctLimpio").asInstanceOf[Double]).headOption.map(_("tecnica").asInstanceOf[String])
      Map("total" -> total, "porTecnica" -> porTecnica, "masUsada" -> masUsada, "mejorTasaLimpia" -> mejorTasa)
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE E — HITOS AUTOMATICOS DE CARRERA
  // ─────────────────────────────────────────────────────────────────────────────
  // SQL puro — se llama desde un hilo de fondo al guardar un partido o un entrenamiento.
  def detectarHitos(): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      def cuenta(sql: String): Int = { val rs = conn.createStatement().executeQuery(sql); if (rs.next()) rs.getInt(1) else 0 }
      def existe(sql: String): Boolean = cuenta(sql) > 0

      // (tipo, descripcion, se_cumple, contexto)
      val candidatos = scala.collection.mutable.ListBuffer[(String, String, Boolean, String)]()

      candidatos += (("PRIMERA_PORTERIA_CERO", "Primera portería a cero de la carrera",
        existe("SELECT 1 FROM matches WHERE status='PLAYED' AND goles_contra=0"), ""))

      val pj = cuenta("SELECT COUNT(*) FROM matches WHERE status='PLAYED'")
      candidatos += (("PARTIDO_10", "10º partido registrado", pj >= 10, ""))
      candidatos += (("PARTIDO_50", "50º partido registrado", pj >= 50, ""))
      candidatos += (("PARTIDO_100", "100º partido registrado", pj >= 100, ""))

      candidatos += (("PRIMERA_NOTA_9", "Primera nota de 9 o superior",
        existe("SELECT 1 FROM matches WHERE status='PLAYED' AND nota >= 9"), ""))

      candidatos += (("CINCO_PARADAS_PARTIDO", "Primer partido con 5 o más paradas",
        existe("SELECT 1 FROM matches WHERE status='PLAYED' AND paradas >= 5"), ""))

      val rsUlt3 = conn.createStatement().executeQuery(
        "SELECT goles_contra FROM matches WHERE status='PLAYED' ORDER BY fecha DESC LIMIT 3")
      val ult3 = scala.collection.mutable.ListBuffer[Int]()
      while (rsUlt3.next()) ult3 += rsUlt3.getInt("goles_contra")
      candidatos += (("TRES_LIMPIAS_SEGUIDAS", "Tres porterías a cero consecutivas",
        ult3.size == 3 && ult3.forall(_ == 0), ""))

      // ACWR en zona verde (acwr_score >= 8) durante el ultimo mes natural completo
      val rsAcwrMes = conn.createStatement().executeQuery("""
        SELECT COUNT(*) as dias, AVG(acwr_score) as media, MIN(acwr_score) as minimo
        FROM forma_diaria
        WHERE fecha >= date_trunc('month', CURRENT_DATE - INTERVAL '1 month')
          AND fecha < date_trunc('month', CURRENT_DATE)
      """)
      val acwrMesOk = if (rsAcwrMes.next()) rsAcwrMes.getInt("dias") >= 20 && rsAcwrMes.getDouble("minimo") >= 8.0 else false
      candidatos += (("ACWR_VERDE_MES", "Primer mes completo con ACWR en zona verde", acwrMesOk, ""))

      // Temporada cerrada mas reciente sin ningun registro de LESION durante su rango de fechas
      val rsTempCerrada = conn.createStatement().executeQuery(
        "SELECT id, COALESCE(nombre, categoria, 'Temporada') as nombre, fecha_inicio, fecha_fin FROM seasons WHERE fecha_fin IS NOT NULL ORDER BY fecha_fin DESC LIMIT 1")
      if (rsTempCerrada.next()) {
        val nombreTemp = fixEncoding(rsTempCerrada.getString("nombre"))
        val fi = rsTempCerrada.getDate("fecha_inicio"); val ff = rsTempCerrada.getDate("fecha_fin")
        val sinLesion = fi != null && ff != null && {
          val ps = conn.prepareStatement("SELECT COUNT(*) FROM wellness WHERE estado_fisico='LESION' AND fecha >= ? AND fecha <= ?")
          ps.setDate(1, fi); ps.setDate(2, ff)
          val r = ps.executeQuery(); (if (r.next()) r.getInt(1) else 0) == 0
        }
        candidatos += (("TEMPORADA_SIN_LESION", "Primera temporada completa sin lesiones", sinLesion, nombreTemp))

        val rsNotaMedia = conn.prepareStatement("SELECT AVG(nota) as m FROM matches WHERE status='PLAYED' AND season_id=?")
        rsNotaMedia.setInt(1, rsTempCerrada.getInt("id"))
        val rNota = rsNotaMedia.executeQuery()
        val notaMediaOk = rNota.next() && rNota.getDouble("m") >= 7.0
        candidatos += (("NOTA_MEDIA_7", "Primera temporada con nota media ≥7", notaMediaOk, nombreTemp))
      }

      val rsSkills = conn.createStatement().executeQuery(
        "SELECT COUNT(*) as total, COUNT(CASE WHEN conseguido THEN 1 END) as conseguidas FROM goalkeeper_skills")
      val (totalSkills, conseguidas) = if (rsSkills.next()) (rsSkills.getInt("total"), rsSkills.getInt("conseguidas")) else (0, 0)
      val pctSkills = if (totalSkills > 0) conseguidas * 100.0 / totalSkills else 0.0
      candidatos += (("CHECKLIST_50PCT", "50% del checklist de habilidades conseguido", totalSkills > 0 && pctSkills >= 50.0, ""))
      candidatos += (("CHECKLIST_100PCT", "100% del checklist de habilidades conseguido", totalSkills > 0 && pctSkills >= 100.0, ""))

      candidatos += (("PRIMERA_ACADEMIA_FEEDBACK", "Primer feedback del entrenador de academia registrado",
        existe("SELECT 1 FROM trainings WHERE feedback_entrenador IS NOT NULL AND feedback_entrenador != ''"), ""))

      // Inserta solo los que se cumplen — el UNIQUE(tipo) + ON CONFLICT DO NOTHING evita duplicados
      var nuevos = List[Map[String, Any]]()
      candidatos.filter(_._3).foreach { case (tipo, descripcion, _, contexto) =>
        val ps = conn.prepareStatement(
          "INSERT INTO hitos_conseguidos (tipo, descripcion, contexto) VALUES (?,?,?) ON CONFLICT (tipo) DO NOTHING")
        ps.setString(1, tipo); ps.setString(2, descripcion); ps.setString(3, contexto)
        if (ps.executeUpdate() > 0) {
          val fecha = LocalDate.now().toString
          nuevos = nuevos :+ Map("tipo" -> tipo, "descripcion" -> descripcion, "fecha" -> fecha, "contexto" -> contexto)
          TelegramService.enviar(s"🏆 NUEVO HITO: $descripcion — $fecha")
        }
      }
      nuevos
    } finally { conn.close() }
  }

  def getHitosRecientes(dias: Int = 7): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement(
        "SELECT tipo, descripcion, fecha, contexto FROM hitos_conseguidos WHERE fecha >= CURRENT_DATE - ? ORDER BY fecha DESC")
      ps.setInt(1, dias)
      val rs = ps.executeQuery()
      var l = List[Map[String, Any]]()
      while (rs.next()) l = l :+ Map(
        "tipo" -> rs.getString("tipo"), "descripcion" -> fixEncoding(rs.getString("descripcion")),
        "fecha" -> rs.getDate("fecha").toString, "contexto" -> fixEncoding(Option(rs.getString("contexto")).getOrElse(""))
      )
      l
    } finally { conn.close() }
  }

  def getTodosLosHitos(): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT tipo, descripcion, fecha, contexto FROM hitos_conseguidos ORDER BY fecha ASC")
      var l = List[Map[String, Any]]()
      while (rs.next()) l = l :+ Map(
        "tipo" -> rs.getString("tipo"), "descripcion" -> fixEncoding(rs.getString("descripcion")),
        "fecha" -> rs.getDate("fecha").toString, "contexto" -> fixEncoding(Option(rs.getString("contexto")).getOrElse(""))
      )
      l
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE G — PROYECCION DE CARGA PROXIMA SEMANA
  // ─────────────────────────────────────────────────────────────────────────────
  // SQL/matematicas puras — sin Gemini.
  private def cargaEstimadaSesion(tipo: String): Double = tipo match {
    case "DESCANSO"     => 0.0
    case "JUDO"         => 60 * 5
    case "EQUIPO"       => 60 * 7
    case "ACADEMIA"     => 60 * 8
    case "PARTIDO"      => 45 * 4
    case "TORNEO"       => 2 * 45 * 4
    case "DOBLE_SESION" => 60 * 7 + 60 * 8
    case _              => 0.0
  }

  /** Carga real diaria (partidos+entrenos) de los ultimos `dias` dias, ordenada de mas antiguo a mas reciente (hoy incluido al final). */
  private def cargasDiariasRecientes(dias: Int): List[Double] = {
    val conn = getConnection()
    try {
      val hoy = LocalDate.now()
      val porDia = scala.collection.mutable.Map[String, Double]().withDefaultValue(0.0)
      val rsM = conn.prepareStatement("SELECT fecha, minutos FROM matches WHERE status='PLAYED' AND fecha >= CURRENT_DATE - ?")
      rsM.setInt(1, dias - 1)
      val rM = rsM.executeQuery()
      while (rM.next()) porDia(rM.getDate("fecha").toString) += rM.getInt("minutos") * 4.0
      val rsT = conn.prepareStatement("SELECT fecha, rpe, fb_distancia FROM trainings WHERE fecha >= CURRENT_DATE - ?")
      rsT.setInt(1, dias - 1)
      val rT = rsT.executeQuery()
      while (rT.next()) {
        val fb = rT.getDouble("fb_distancia")
        porDia(rT.getDate("fecha").toString) += 60 * rT.getInt("rpe") * (1 + fb * 0.05)
      }
      (dias - 1 to 0 by -1).map(offset => porDia(hoy.minusDays(offset).toString)).toList
    } finally { conn.close() }
  }

  /**
   * Proyecta el ACWR dia a dia de la proxima semana. `sesionesProximas` mapea el nombre del dia
   * (LUNES..DOMINGO) al tipo de sesion prevista. La cronica usa la carga real de las ultimas 4
   * semanas; la aguda combina los dias reales que aun quedan en la ventana movil de 7 dias con
   * las sesiones proyectadas.
   */
  def proyectarACWR(sesionesProximas: Map[String, String], cargaHoyPendiente: Double = 0.0): Map[String, Any] = {
    val diasSemana = Seq("LUNES", "MARTES", "MIERCOLES", "JUEVES", "VIERNES", "SABADO", "DOMINGO")
    // cargaHoyPendiente: sesion prevista hoy que aun no se ha registrado (p.ej. Judo a las 21:00)
    val real7 = cargasDiariasRecientes(7) match { case l if l.nonEmpty => l.init :+ (l.last + cargaHoyPendiente); case l => l }
    val proyectado = diasSemana.map(d => cargaEstimadaSesion(sesionesProximas.getOrElse(d, "DESCANSO"))).toList
    val combinado = real7 ++ proyectado // 14 valores: dias -6..0 (reales) + dias +1..+7 (proyectados)

    val cargaCronica28 = getWorkloads(28).sum
    val cronicaSemanalEquivalente = if (cargaCronica28 > 0) cargaCronica28 / 4.0 else 0.0

    val faseBio = try getBioBandingData().getOrElse("faseBio", "").toString catch { case _: Exception => "" }
    val circaPhvActivo = faseBio.contains("PICO ACTIVO")

    val u = umbralesACWR()
    def semaforo(acwr: Double): String =
      if (acwr <= 0.0) "verde"
      else if (acwr < u.optimoMin) "amarillo"
      else if (acwr <= u.optimoMax) "verde"
      else if (acwr <= u.riesgo) "naranja"
      else "rojo"

    var alertas = List[String]()
    val dias = (1 to 7).map { i =>
      val ventana = combinado.slice(i, i + 7)
      val acuteSemanal = ventana.sum
      val acwr = if (cronicaSemanalEquivalente > 0) acuteSemanal / cronicaSemanalEquivalente else 0.0
      val nombreDia = diasSemana(i - 1)
      val tipoSesion = sesionesProximas.getOrElse(nombreDia, "DESCANSO")
      val sem = semaforo(acwr)

      if (acwr > u.riesgo) {
        val diaAnterior = if (i > 1) diasSemana(i - 2) else "domingo anterior"
        alertas = alertas :+ s"⚠️ El $nombreDia proyecta sobrecarga (ACWR ${"%.2f".format(acwr)}). Considera reducir la sesión del $diaAnterior."
      }
      if (circaPhvActivo && acwr > u.optimoMax) {
        alertas = alertas :+ s"🔴 PRIORIDAD MÁXIMA: Héctor está en pico activo de crecimiento (Circa-PHV) y el $nombreDia proyecta ACWR ${"%.2f".format(acwr)}. Riesgo de lesión elevado — considera aligerar esa sesión."
      }

      Map("dia" -> nombreDia, "tipoSesion" -> tipoSesion, "acwr" -> acwr, "semaforo" -> sem)
    }.toList

    Map("dias" -> dias, "alertas" -> alertas, "circaPhvActivo" -> circaPhvActivo, "cronicaSemanalEquivalente" -> cronicaSemanalEquivalente)
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE I — FIRMA DE FATIGA PERSONAL
  // ─────────────────────────────────────────────────────────────────────────────
  // SQL/matematicas puras — sin Gemini. Requiere >=8 partidos con ACWR alto (acwr_score<6) y rubrica completa.
  def getFirmaFatiga(): Map[String, Any] = {
    val conn = getConnection()
    try {
      val rsCansado = conn.createStatement().executeQuery("""
        SELECT AVG(m.rubrica_posicion) as pos, AVG(m.rubrica_decisiones) as dec,
               AVG(m.rubrica_pies) as pies, AVG(m.rubrica_comunicacion) as com,
               AVG(m.rubrica_actitud) as act, COUNT(*) as n
        FROM matches m
        JOIN forma_diaria f ON f.fecha = m.fecha::date
        WHERE m.status='PLAYED' AND f.acwr_score < 6 AND m.rubrica_posicion IS NOT NULL
      """)
      if (!rsCansado.next()) return Map("suficiente" -> false, "n" -> 0)
      val n = rsCansado.getInt("n")
      if (n < 8) return Map("suficiente" -> false, "n" -> n)

      val dimensionesCansado = Map(
        "Posición" -> rsCansado.getDouble("pos"), "Decisiones bajo presión" -> rsCansado.getDouble("dec"),
        "Juego con los pies" -> rsCansado.getDouble("pies"), "Comunicación" -> rsCansado.getDouble("com"),
        "Actitud y concentración" -> rsCansado.getDouble("act")
      )

      val rsNormal = conn.createStatement().executeQuery("""
        SELECT AVG(rubrica_posicion) as pos, AVG(rubrica_decisiones) as dec, AVG(rubrica_pies) as pies,
               AVG(rubrica_comunicacion) as com, AVG(rubrica_actitud) as act
        FROM matches WHERE status='PLAYED' AND rubrica_posicion IS NOT NULL
      """)
      rsNormal.next()
      val dimensionesNormal = Map(
        "Posición" -> rsNormal.getDouble("pos"), "Decisiones bajo presión" -> rsNormal.getDouble("dec"),
        "Juego con los pies" -> rsNormal.getDouble("pies"), "Comunicación" -> rsNormal.getDouble("com"),
        "Actitud y concentración" -> rsNormal.getDouble("act")
      )

      val comparativa = dimensionesNormal.keys.map { dim =>
        val normal = dimensionesNormal(dim); val cansado = dimensionesCansado(dim)
        Map("dimension" -> dim, "normal" -> normal, "cansado" -> cansado, "diferencia" -> (normal - cansado))
      }.toList.sortBy(m => -m("diferencia").asInstanceOf[Double])

      val firmaFatiga = comparativa.headOption.map(_("dimension").asInstanceOf[String]).getOrElse("")

      Map("suficiente" -> true, "n" -> n, "comparativa" -> comparativa, "firmaFatiga" -> firmaFatiga)
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE J — RATIO DE ENTRENAMIENTO ESPECIFICO DE PORTERO
  // ─────────────────────────────────────────────────────────────────────────────
  // SQL/matematicas puras — sin Gemini.
  def getRatioEntrenamientoEspecifico(seasonId: Int = 0): Map[String, Any] = {
    val conn = getConnection()
    try {
      val efectivo = if (seasonId > 0) seasonId else getTemporadaActivaId()
      val rsFechas = conn.prepareStatement("SELECT fecha_inicio, fecha_fin FROM seasons WHERE id=?")
      rsFechas.setInt(1, efectivo)
      val rf = rsFechas.executeQuery()
      val (fi, ff) = if (rf.next())
        (Option(rf.getDate("fecha_inicio")).map(_.toString).getOrElse("2000-01-01"),
         Option(rf.getDate("fecha_fin")).map(_.toString).getOrElse(LocalDate.now().toString))
      else ("2000-01-01", LocalDate.now().toString)

      val ps = conn.prepareStatement("""
        SELECT
          COUNT(CASE WHEN tipo = 'Academia' THEN 1 END) as especifico,
          COUNT(CASE WHEN tipo = 'Club' THEN 1 END) as colectivo,
          COUNT(CASE WHEN tipo = 'Judo' THEN 1 END) as complementario,
          COUNT(*) as total
        FROM trainings
        WHERE fecha >= ?::date AND fecha <= ?::date AND tipo_ausencia IS NULL
      """)
      ps.setString(1, fi); ps.setString(2, ff)
      val rs = ps.executeQuery()
      rs.next()
      val especifico = rs.getInt("especifico"); val colectivo = rs.getInt("colectivo")
      val complementario = rs.getInt("complementario"); val total = rs.getInt("total")
      val ratioEspecifico = if (total > 0) especifico * 100.0 / total else 0.0

      val card = getLatestCardData()
      val edad = calcularEdadExacta(card.fechaNacimiento)
      val (recMin, recMax) =
        if (edad <= 8) (15.0, 20.0)
        else if (edad <= 10) (25.0, 30.0)
        else if (edad <= 12) (35.0, 40.0)
        else (45.0, 55.0)

      Map(
        "especifico" -> especifico, "colectivo" -> colectivo, "complementario" -> complementario, "total" -> total,
        "ratioEspecifico" -> ratioEspecifico, "edad" -> edad, "recMin" -> recMin, "recMax" -> recMax,
        "porDebajo" -> (ratioEspecifico < recMin), "porEncima" -> (ratioEspecifico >= recMin)
      )
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE RFFM — BENCHMARKING REAL CONTRA LA CATEGORIA (Prebenjamin F7, RFFM Madrid)
  // ─────────────────────────────────────────────────────────────────────────────
  // AVISO: rffm.es no publica un contrato de API estable — los nombres de campo del
  // JSON embebido se infieren de forma defensiva (varios candidatos por campo, escaneo
  // recursivo para localizar arrays de partidos). Si rffm.es cambia su HTML/JSON, el
  // sync fallara con gracia (log + contador de fallos) sin afectar al resto de Guardian.

  def getRffmCompeticionId(): String = {
    val conn = getConnection()
    try {
      val rs = conn.prepareStatement("SELECT payload FROM feature_cache WHERE cache_key='rffm_competicion_id_override'")
      val r = rs.executeQuery()
      if (r.next()) r.getString("payload") else sys.env.getOrElse("RFFM_COMPETICION_ID", "26738167")
    } finally { conn.close() }
  }

  def getRffmTemporada(): String = {
    val conn = getConnection()
    try {
      val rs = conn.prepareStatement("SELECT payload FROM feature_cache WHERE cache_key='rffm_temporada_override'")
      val r = rs.executeQuery()
      if (r.next()) r.getString("payload") else sys.env.getOrElse("RFFM_TEMPORADA", "22")
    } finally { conn.close() }
  }

  def setRffmConfig(competicionId: String, temporada: String): Unit = {
    val conn = getConnection()
    try {
      def upsert(key: String, value: String): Unit = {
        val ps = conn.prepareStatement(
          "INSERT INTO feature_cache (cache_key, payload, updated_at) VALUES (?,?,NOW()) ON CONFLICT (cache_key) DO UPDATE SET payload=EXCLUDED.payload, updated_at=NOW()")
        ps.setString(1, key); ps.setString(2, value); ps.executeUpdate()
      }
      if (competicionId.trim.nonEmpty) upsert("rffm_competicion_id_override", competicionId.trim)
      if (temporada.trim.nonEmpty) upsert("rffm_temporada_override", temporada.trim)
    } finally { conn.close() }
  }

  private def rffmSyncEstado(estado: String): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement(
        "INSERT INTO feature_cache (cache_key, payload, updated_at) VALUES ('rffm_sync_status', ?, NOW()) ON CONFLICT (cache_key) DO UPDATE SET payload=EXCLUDED.payload, updated_at=NOW()")
      ps.setString(1, estado); ps.executeUpdate()
    } finally { conn.close() }
  }

  def getRffmSyncEstado(): String = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("SELECT payload FROM feature_cache WHERE cache_key='rffm_sync_status'")
      if (rs.next()) rs.getString("payload") else "Sin sincronizar todavia"
    } finally { conn.close() }
  }

  private def rffmFailCount(): Int = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("SELECT payload FROM feature_cache WHERE cache_key='rffm_fail_count'")
      if (rs.next()) rs.getString("payload").toIntOption.getOrElse(0) else 0
    } finally { conn.close() }
  }
  private def rffmFailCountSet(n: Int): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement(
        "INSERT INTO feature_cache (cache_key, payload, updated_at) VALUES ('rffm_fail_count', ?, NOW()) ON CONFLICT (cache_key) DO UPDATE SET payload=EXCLUDED.payload, updated_at=NOW()")
      ps.setString(1, n.toString); ps.executeUpdate()
    } finally { conn.close() }
  }

  /** Descarga una URL de rffm.es y extrae el JSON embebido, probando varias estrategias. */
  private def rffmFetchJson(url: String): Option[ujson.Value] = {
    try {
      val doc = Jsoup.connect(url)
        .userAgent("Mozilla/5.0 (compatible; Guardian/1.0)")
        .timeout(15000)
        .get()

      def tryParse(s: String): Option[ujson.Value] =
        if (s == null || s.isEmpty) None else try Some(ujson.read(s)) catch { case _: Exception => None }

      // Estrategia 1: <script> que contiene "grupos_competicion"
      val scripts = doc.select("script").asScala
      val jsonScript = scripts.find(s => s.html().contains("grupos_competicion"))
      val desdeScript = jsonScript.flatMap { s =>
        val html = s.html()
        val startIdx = html.indexOf("{\"")
        val endIdx = html.lastIndexOf("}") + 1
        if (startIdx >= 0 && endIdx > startIdx) tryParse(html.substring(startIdx, endIdx)) else None
      }
      if (desdeScript.isDefined) return desdeScript

      // Estrategia 2: atributo data-* con el JSON
      val elemento = doc.select("[data-results], [data-json], #resultados-data").first()
      val desdeAtributo = Option(elemento).flatMap { e =>
        val raw = if (e.hasAttr("data-results")) e.attr("data-results")
                  else if (e.hasAttr("data-json")) e.attr("data-json")
                  else e.attr("data-results")
        tryParse(raw)
      }
      if (desdeAtributo.isDefined) return desdeAtributo

      // Estrategia 3: buscar "grupos_competicion" en el HTML completo y extraer manualmente
      val htmlCompleto = doc.html()
      val idx = htmlCompleto.indexOf("grupos_competicion")
      if (idx >= 0) {
        // Retrocede hasta la llave que abre el objeto que contiene esa clave
        val startIdx = htmlCompleto.lastIndexOf("{\"", idx)
        val endIdx = htmlCompleto.lastIndexOf("}") + 1
        if (startIdx >= 0 && endIdx > startIdx) tryParse(htmlCompleto.substring(startIdx, endIdx)) else None
      } else None
    } catch { case e: Exception =>
      if (debugMode) println(s"[RFFM] Error descargando $url: ${e.getMessage}")
      None
    }
  }

  /** Prueba varias claves candidatas sobre un objeto JSON y devuelve la primera que exista como String. */
  private def jsonStrCandidatos(obj: ujson.Value, claves: Seq[String]): Option[String] =
    claves.iterator.flatMap { k =>
      try {
        val v = obj(k)
        if (v.isNull) None
        else Some(scala.util.Try(v.str).getOrElse(v.num.toLong.toString))
      } catch { case _: Exception => None }
    }.nextOption()

  /** Busca recursivamente en el JSON un array cuyo cache_key sea exactamente `nombre`. */
  private def buscarArrayPorClave(v: ujson.Value, nombre: String): Option[List[ujson.Value]] = {
    v match {
      case o: ujson.Obj =>
        o.value.get(nombre) match {
          case Some(a: ujson.Arr) => Some(a.value.toList)
          case _ => o.value.values.iterator.map(x => buscarArrayPorClave(x, nombre)).find(_.isDefined).flatten
        }
      case a: ujson.Arr =>
        a.value.iterator.map(x => buscarArrayPorClave(x, nombre)).find(_.isDefined).flatten
      case _ => None
    }
  }

  /** Busca recursivamente el primer array cuyos elementos parezcan partidos (tienen campos de goles). */
  private def buscarArrayPartidos(v: ujson.Value): Option[List[ujson.Value]] = {
    val clavesGol = Seq("goles_local", "goles_visita", "goles_visitante", "resultado_local", "golesLocal")
    v match {
      case a: ujson.Arr if a.value.nonEmpty && a.value.head.isInstanceOf[ujson.Obj] &&
        clavesGol.exists(k => a.value.head.asInstanceOf[ujson.Obj].value.contains(k)) =>
        Some(a.value.toList)
      case o: ujson.Obj =>
        o.value.values.iterator.map(buscarArrayPartidos).find(_.isDefined).flatten
      case a: ujson.Arr =>
        a.value.iterator.map(buscarArrayPartidos).find(_.isDefined).flatten
      case _ => None
    }
  }

  /**
   * Sincroniza los resultados de la categoria Prebenjamin F7 desde rffm.es. Sincrona — SIEMPRE
   * debe llamarse desde un hilo de fondo (ver syncRFFMBenchmarkAsync). Devuelve un resumen textual.
   */
  def syncRFFMBenchmark(): String = {
    rffmSyncEstado("IN_PROGRESS")
    val competicion = getRffmCompeticionId()
    val temporada = getRffmTemporada()
    val baseUrl = "https://www.rffm.es/competicion/resultados-y-jornadas"

    try {
      // Paso 1: descubrir todos los grupos de la competicion a partir de una jornada de referencia
      val urlInicial = s"$baseUrl?temporada=$temporada&competicion=$competicion&grupo=26738199&jornada=1&tipojuego=2"
      val jsonInicial = rffmFetchJson(urlInicial)
      if (jsonInicial.isEmpty) {
        rffmFailCountSet(rffmFailCount() + 1)
        rffmSyncEstado(s"ERROR: no se pudo leer el JSON inicial de rffm.es (${LocalDate.now()})")
        return "Error: no se pudo conectar con rffm.es"
      }

      val gruposJson = buscarArrayPorClave(jsonInicial.get, "grupos_competicion").getOrElse(List.empty)
      val grupos: List[(String, String)] = gruposJson.flatMap { g =>
        for {
          id <- jsonStrCandidatos(g, Seq("id", "grupo", "grupo_id", "idgrupo", "codigo"))
          nombre <- jsonStrCandidatos(g, Seq("nombre", "descripcion", "name", "grupo_nombre")).orElse(Some(id))
        } yield (id, nombre)
      }
      val gruposFinal = if (grupos.nonEmpty) grupos else List(("26738199", "Grupo principal"))

      var totalPartidosNuevos = 0
      // Una conexion por grupo (no una sola para todo el sync) — un sync con muchos grupos
      // puede tardar varios minutos y no conviene retener una conexion del pool tanto tiempo.
      gruposFinal.foreach { case (grupoId, nombreGrupo) =>
        val conn = getConnection()
        try {
          var jornada = 1
          var continuar = true
          while (continuar && jornada <= 30) {
            val url = s"$baseUrl?temporada=$temporada&competicion=$competicion&grupo=$grupoId&jornada=$jornada&tipojuego=2"
            rffmFetchJson(url) match {
              case None => continuar = false
              case Some(json) =>
                val partidos = buscarArrayPartidos(json).getOrElse(List.empty)
                if (partidos.isEmpty) continuar = false
                else {
                  partidos.foreach { p =>
                    val local = jsonStrCandidatos(p, Seq("equipo_local", "local", "equipoLocal", "nombre_local"))
                    val visita = jsonStrCandidatos(p, Seq("equipo_visitante", "equipo_visita", "visitante", "equipoVisitante", "nombre_visitante"))
                    val gLocal = jsonStrCandidatos(p, Seq("goles_local", "golesLocal", "resultado_local")).flatMap(_.toIntOption)
                    val gVisita = jsonStrCandidatos(p, Seq("goles_visita", "goles_visitante", "golesVisitante", "resultado_visitante")).flatMap(_.toIntOption)
                    val fecha = jsonStrCandidatos(p, Seq("fecha", "fecha_partido", "dia"))
                    (local, visita, gLocal, gVisita) match {
                      case (Some(l), Some(v), Some(gl), Some(gv)) =>
                        val ps = conn.prepareStatement("""
                          INSERT INTO rffm_benchmark (temporada, competicion, grupo, nombre_grupo, equipo_local, equipo_visita, goles_local, goles_visita, jornada, fecha)
                          VALUES (?,?,?,?,?,?,?,?,?,?::date)
                          ON CONFLICT (competicion, grupo, equipo_local, equipo_visita, jornada) DO NOTHING
                        """)
                        ps.setString(1, temporada); ps.setString(2, competicion); ps.setString(3, grupoId); ps.setString(4, fixEncoding(nombreGrupo))
                        ps.setString(5, fixEncoding(l)); ps.setString(6, fixEncoding(v)); ps.setInt(7, gl); ps.setInt(8, gv); ps.setInt(9, jornada)
                        fecha.flatMap(f => scala.util.Try(LocalDate.parse(f).toString).toOption) match {
                          case Some(f) => ps.setString(10, f)
                          case None => ps.setNull(10, java.sql.Types.DATE)
                        }
                        if (ps.executeUpdate() > 0) totalPartidosNuevos += 1
                      case _ => ()
                    }
                  }
                  jornada += 1
                  Thread.sleep(300) // buen ciudadano: no saturar rffm.es
                }
            }
          }
        } finally { conn.close() }
      }

      calcularPercentilesRFFM()
      // BLOQUE A6: si la temporada activa juega liga oficial RFMF, detecta resultados nuevos
      val pendientesDetectados = try detectarPartidosRFMF() catch { case e: Exception => if (debugMode) println(s"[RFFM] detectarPartidosRFMF error: ${e.getMessage}"); List.empty }
      if (pendientesDetectados.nonEmpty) TelegramService.enviar(s"📋 RFMF: ${pendientesDetectados.size} resultado(s) nuevo(s) detectados — confírmalos en Guardian.")
      rffmFailCountSet(0)
      val msg = s"✅ Sync RFFM completado: $totalPartidosNuevos partidos nuevos en ${gruposFinal.size} grupos (${LocalDate.now()})"
      rffmSyncEstado(msg)
      msg
    } catch { case e: Exception =>
      rffmFailCountSet(rffmFailCount() + 1)
      val msg = s"ERROR: ${e.getMessage}"
      rffmSyncEstado(msg)
      if (debugMode) e.printStackTrace()
      msg
    }
  }

  /** Lanza el sync en background — nunca bloquea al llamador (arranque del servidor, boton admin, cron). */
  def syncRFFMBenchmarkAsync(): Unit = {
    new Thread(() => { try syncRFFMBenchmark() catch { case e: Exception => println(s"[RFFM] sync async error: ${e.getMessage}") } }).start()
  }

  def calcularPercentilesRFFM(): Unit = {
    val conn = getConnection()
    try {
      val temporada = getRffmTemporada()
      val competicion = getRffmCompeticionId()
      val ps = conn.prepareStatement(s"""
        WITH gc_por_equipo AS (
          SELECT equipo_local as equipo, goles_visita as gc FROM rffm_benchmark WHERE temporada = ? AND competicion = ?
          UNION ALL
          SELECT equipo_visita as equipo, goles_local as gc FROM rffm_benchmark WHERE temporada = ? AND competicion = ?
        ),
        stats_equipo AS (
          SELECT equipo, AVG(gc) as media_gc,
            SUM(CASE WHEN gc = 0 THEN 1 ELSE 0 END)::float / COUNT(*) as pct_limpias,
            COUNT(*) as partidos
          FROM gc_por_equipo
          GROUP BY equipo
          HAVING COUNT(*) >= 3
        )
        SELECT
          COUNT(*) as total_equipos,
          COALESCE(SUM(partidos), 0) as total_partidos,
          COALESCE(AVG(media_gc), 0) as media_global,
          COALESCE(PERCENTILE_CONT(0.10) WITHIN GROUP (ORDER BY media_gc), 0) as p10,
          COALESCE(PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY media_gc), 0) as p25,
          COALESCE(PERCENTILE_CONT(0.50) WITHIN GROUP (ORDER BY media_gc), 0) as p50,
          COALESCE(PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY media_gc), 0) as p75,
          COALESCE(PERCENTILE_CONT(0.90) WITHIN GROUP (ORDER BY media_gc), 0) as p90,
          COALESCE(AVG(pct_limpias), 0) as pct_limpias_media
        FROM stats_equipo
      """)
      ps.setString(1, temporada); ps.setString(2, competicion); ps.setString(3, temporada); ps.setString(4, competicion)
      val rs = ps.executeQuery()
      if (rs.next() && rs.getInt("total_equipos") > 0) {
        val ins = conn.prepareStatement("""
          INSERT INTO rffm_percentiles (temporada, competicion, total_partidos, total_equipos, media_gc, p10_gc, p25_gc, p50_gc, p75_gc, p90_gc, pct_limpias)
          VALUES (?,?,?,?,?,?,?,?,?,?,?)
        """)
        ins.setString(1, temporada); ins.setString(2, competicion)
        ins.setInt(3, rs.getInt("total_partidos")); ins.setInt(4, rs.getInt("total_equipos"))
        ins.setDouble(5, rs.getDouble("media_global")); ins.setDouble(6, rs.getDouble("p10")); ins.setDouble(7, rs.getDouble("p25"))
        ins.setDouble(8, rs.getDouble("p50")); ins.setDouble(9, rs.getDouble("p75")); ins.setDouble(10, rs.getDouble("p90"))
        ins.setDouble(11, rs.getDouble("pct_limpias_media"))
        ins.executeUpdate()
      }
    } finally { conn.close() }
  }

  private def getUltimoPercentilRFFM(conn: Connection): Option[Map[String, Any]] = {
    val rs = conn.createStatement().executeQuery(
      "SELECT * FROM rffm_percentiles ORDER BY fecha_calculo DESC, id DESC LIMIT 1")
    if (!rs.next()) None
    else Some(Map(
      "totalEquipos" -> rs.getInt("total_equipos"), "totalPartidos" -> rs.getInt("total_partidos"),
      "mediaGc" -> rs.getDouble("media_gc"), "p10" -> rs.getDouble("p10_gc"), "p25" -> rs.getDouble("p25_gc"),
      "p50" -> rs.getDouble("p50_gc"), "p75" -> rs.getDouble("p75_gc"), "p90" -> rs.getDouble("p90_gc"),
      "pctLimpiasCategoria" -> (rs.getDouble("pct_limpias") * 100.0), "fechaCalculo" -> rs.getDate("fecha_calculo").toString
    ))
  }

  // BLOQUE A6/A7: configuracion de liga oficial RFMF de la temporada activa
  def getLigaRFMFConfig(): Map[String, Any] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT liga_tipo, rffm_nombre_equipo, rffm_grupo_id FROM seasons WHERE fecha_fin IS NULL ORDER BY id DESC LIMIT 1")
      if (rs.next()) Map(
        "ligaTipo" -> Option(rs.getString("liga_tipo")).getOrElse("INTERNA"),
        "nombreEquipo" -> Option(rs.getString("rffm_nombre_equipo")).getOrElse(""),
        "grupoId" -> Option(rs.getString("rffm_grupo_id")).getOrElse("")
      ) else Map("ligaTipo" -> "INTERNA", "nombreEquipo" -> "", "grupoId" -> "")
    } finally { conn.close() }
  }

  def setLigaRFMFConfig(ligaTipo: String, nombreEquipo: String, grupoId: String): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement(
        "UPDATE seasons SET liga_tipo=?, rffm_nombre_equipo=?, rffm_grupo_id=? WHERE id = (SELECT id FROM seasons WHERE fecha_fin IS NULL ORDER BY id DESC LIMIT 1)")
      ps.setString(1, if (ligaTipo.nonEmpty) ligaTipo else "INTERNA")
      if (nombreEquipo.trim.nonEmpty) ps.setString(2, fixEncoding(nombreEquipo.trim)) else ps.setNull(2, java.sql.Types.VARCHAR)
      if (grupoId.trim.nonEmpty) ps.setString(3, grupoId.trim) else ps.setNull(3, java.sql.Types.VARCHAR)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def getRffmFechaCalculo(): Option[String] = {
    val conn = getConnection()
    try getUltimoPercentilRFFM(conn).map(_("fechaCalculo").asInstanceOf[String]) finally conn.close()
  }

  /** Percentil real de Hector frente a la categoria. None si no hay percentiles calculados (>=10 equipos). */
  def getPercentilRealHector(seasonId: Int = 0): Option[Map[String, Any]] = {
    val conn = getConnection()
    try {
      getUltimoPercentilRFFM(conn).filter(_("totalEquipos").asInstanceOf[Int] >= 10).map { cat =>
        val efectivo = if (seasonId > 0) seasonId else getTemporadaActivaId()
        val rsH = conn.prepareStatement(s"""
          SELECT COUNT(*) as pj, COALESCE(AVG(goles_contra),0) as media_gc,
            SUM(CASE WHEN goles_contra=0 THEN 1 ELSE 0 END)::float / NULLIF(COUNT(*),0) as pct_limpias
          FROM matches WHERE status='PLAYED' ${seasonFilter(efectivo)}
        """)
        val rH = rsH.executeQuery(); rH.next()
        val mediaGcHector = rH.getDouble("media_gc")
        val pctLimpiasHector = rH.getDouble("pct_limpias") * 100.0

        val p10 = cat("p10").asInstanceOf[Double]; val p25 = cat("p25").asInstanceOf[Double]
        val p50 = cat("p50").asInstanceOf[Double]; val p75 = cat("p75").asInstanceOf[Double]
        // GC bajo es mejor: menos goles encajados que el p10 de la categoria => percentil alto (elite)
        val percentilGC =
          if (mediaGcHector <= p10) 90
          else if (mediaGcHector <= p25) 75
          else if (mediaGcHector <= p50) 50
          else if (mediaGcHector <= p75) 25
          else 10

        Map(
          "pjHector"         -> rH.getInt("pj"),
          "mediaGcHector"    -> mediaGcHector,
          "percentilGC"      -> percentilGC,
          "mediaCategoria"   -> p50,
          "totalEquipos"     -> cat("totalEquipos").asInstanceOf[Int],
          "totalPartidos"    -> cat("totalPartidos").asInstanceOf[Int],
          "pctLimpiasHector" -> pctLimpiasHector,
          "pctLimpiasCategoria" -> cat("pctLimpiasCategoria").asInstanceOf[Double],
          "fuenteDatos"      -> s"RFFM Prebenjamín F7 Madrid temporada ${getRffmTemporada()}"
        )
      }
    } finally { conn.close() }
  }

  // BLOQUE A6 (RFMF): si la temporada activa juega liga oficial RFMF, detecta resultados
  // del equipo de Hector en rffm_benchmark que aun no esten registrados en matches y los
  // crea como RESULTADO_PENDIENTE para que el padre los confirme. Skip total si liga_tipo='INTERNA'.
  def detectarPartidosRFMF(): List[Int] = {
    val conn = getConnection()
    try {
      val rsSeason = conn.createStatement().executeQuery(
        "SELECT id, liga_tipo, rffm_nombre_equipo FROM seasons WHERE fecha_fin IS NULL ORDER BY id DESC LIMIT 1")
      if (!rsSeason.next()) return List.empty
      val seasonId = rsSeason.getInt("id")
      val ligaTipo = Option(rsSeason.getString("liga_tipo")).getOrElse("INTERNA")
      val equipo = Option(rsSeason.getString("rffm_nombre_equipo")).getOrElse("")
      if (ligaTipo != "RFMF" || equipo.trim.isEmpty) return List.empty

      val patron = s"%${equipo.trim}%"
      val ps = conn.prepareStatement("""
        INSERT INTO matches (season_id, fecha, rival, tipo_partido, status, goles_favor, goles_contra)
        SELECT ?, rb.fecha,
          CASE WHEN LOWER(rb.equipo_local) LIKE LOWER(?) THEN rb.equipo_visita ELSE rb.equipo_local END,
          'LIGA', 'RESULTADO_PENDIENTE',
          CASE WHEN LOWER(rb.equipo_local) LIKE LOWER(?) THEN rb.goles_local ELSE rb.goles_visita END,
          CASE WHEN LOWER(rb.equipo_local) LIKE LOWER(?) THEN rb.goles_visita ELSE rb.goles_local END
        FROM rffm_benchmark rb
        WHERE (LOWER(rb.equipo_local) LIKE LOWER(?) OR LOWER(rb.equipo_visita) LIKE LOWER(?))
          AND rb.fecha IS NOT NULL
          AND NOT EXISTS (SELECT 1 FROM matches m WHERE m.season_id = ? AND m.fecha = rb.fecha)
        RETURNING id
      """)
      ps.setInt(1, seasonId)
      ps.setString(2, patron); ps.setString(3, patron); ps.setString(4, patron)
      ps.setString(5, patron); ps.setString(6, patron)
      ps.setInt(7, seasonId)
      val rs = ps.executeQuery()
      var nuevos = List[Int]()
      while (rs.next()) nuevos = nuevos :+ rs.getInt("id")
      nuevos
    } finally { conn.close() }
  }

  def getPartidosRFMFPendientes(): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT id, rival, fecha, goles_favor, goles_contra FROM matches WHERE status='RESULTADO_PENDIENTE' ORDER BY fecha DESC")
      var l = List[Map[String, Any]]()
      while (rs.next()) l = l :+ Map(
        "id" -> rs.getInt("id"), "rival" -> fixEncoding(rs.getString("rival")), "fecha" -> rs.getDate("fecha").toString,
        "golesFavor" -> rs.getInt("goles_favor"), "golesContra" -> rs.getInt("goles_contra")
      )
      l
    } finally { conn.close() }
  }

  def confirmarPartidoRFMFPendiente(matchId: Int): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("UPDATE matches SET status='PLAYED' WHERE id=? AND status='RESULTADO_PENDIENTE'")
      ps.setInt(1, matchId); ps.executeUpdate()
    } finally { conn.close() }
  }

  def descartarPartidoRFMFPendiente(matchId: Int): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("DELETE FROM matches WHERE id=? AND status='RESULTADO_PENDIENTE'")
      ps.setInt(1, matchId); ps.executeUpdate()
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE H — INDICE DE RENDIMIENTO CONTEXTUAL (CPI)
  // ─────────────────────────────────────────────────────────────────────────────
  // SQL/matematicas puras — sin Gemini.
  def calcularCPI(matchId: Int): Double = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("SELECT rival, nota, clima, es_local, fecha FROM matches WHERE id=?")
      ps.setInt(1, matchId)
      val r = ps.executeQuery()
      if (!r.next()) return 0.0
      val rival = Option(r.getString("rival")).getOrElse("")
      val nota = r.getDouble("nota")
      val clima = Option(r.getString("clima")).getOrElse("")
      val esLocalValue = r.getBoolean("es_local")
      val esFuera = !r.wasNull() && !esLocalValue
      val fecha = r.getDate("fecha").toString

      // Factor rival: comparamos los goles encajados de media contra este rival con la media global
      val rsRival = conn.prepareStatement("SELECT AVG(goles_contra) as m, COUNT(*) as n FROM matches WHERE status='PLAYED' AND LOWER(rival)=LOWER(?)")
      rsRival.setString(1, rival)
      val rrR = rsRival.executeQuery()
      val (mediaRival, nRival) = if (rrR.next()) (rrR.getDouble("m"), rrR.getInt("n")) else (0.0, 0)
      val rsGlobal = conn.createStatement().executeQuery("SELECT AVG(goles_contra) as m FROM matches WHERE status='PLAYED'")
      val mediaGlobal = if (rsGlobal.next()) rsGlobal.getDouble("m") else 0.0
      val factorRival =
        if (nRival < 2 || mediaGlobal <= 0) 1.0
        else if (mediaRival > mediaGlobal * 1.3) 1.2
        else if (mediaRival < mediaGlobal * 0.7) 0.8
        else 1.0

      // Factor condiciones fisicas: indice de forma calculado ese dia
      val rsForma = conn.prepareStatement("SELECT indice_forma FROM forma_diaria WHERE fecha = ?::date")
      rsForma.setString(1, fecha)
      val rf = rsForma.executeQuery()
      val factorForma =
        if (rf.next()) { val forma = rf.getDouble("indice_forma"); if (forma < 5.0) 0.9 else if (forma >= 8.0) 1.1 else 1.0 }
        else 1.0

      val factorClima = if (clima == "Lluvia" || clima.startsWith("Fr")) 1.1 else 1.0
      val factorSede = if (esFuera) 1.05 else 1.0

      val cpi = nota * factorRival * factorForma * factorClima * factorSede
      math.min(10.0, math.max(1.0, cpi))
    } finally { conn.close() }
  }

  // Calcula y guarda el CPI en background tras guardar el partido — nunca bloquea la respuesta.
  def actualizarCPI(matchId: Int): Unit = {
    new Thread(() => {
      try {
        val cpi = calcularCPI(matchId)
        val conn = getConnection()
        try {
          val up = conn.prepareStatement("UPDATE matches SET cpi=? WHERE id=?")
          up.setDouble(1, cpi); up.setInt(2, matchId)
          up.executeUpdate()
        } finally { conn.close() }
      } catch { case e: Exception => println(s"[!] actualizarCPI error: ${e.getMessage}") }
    }).start()
  }

  /** CPI medio de la temporada activa, para usar en getDeepAnalysis() en lugar de la nota media cuando hay datos. */
  def getCpiMedioTemporada(): Option[Double] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        s"SELECT AVG(cpi) as m, COUNT(*) as n FROM matches WHERE status='PLAYED' AND cpi IS NOT NULL ${seasonFilter(getTemporadaActivaId())}")
      if (rs.next() && rs.getInt("n") > 0) Some(rs.getDouble("m")) else None
    } finally { conn.close() }
  }

  def getRubricaMatch(matchId: Int): Option[Map[String, Int]] = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("""
        SELECT rubrica_posicion, rubrica_decisiones, rubrica_pies, rubrica_comunicacion, rubrica_actitud
        FROM matches WHERE id=? AND rubrica_posicion IS NOT NULL""")
      ps.setInt(1, matchId)
      val rs = ps.executeQuery()
      if (rs.next()) Some(Map(
        "posicion" -> rs.getInt("rubrica_posicion"), "decisiones" -> rs.getInt("rubrica_decisiones"),
        "pies" -> rs.getInt("rubrica_pies"), "comunicacion" -> rs.getInt("rubrica_comunicacion"),
        "actitud" -> rs.getInt("rubrica_actitud")
      )) else None
    } finally { conn.close() }
  }

  /** Evolucion de las 5 dimensiones de la rubrica, un punto por partido con rubrica completa. Para /temporal. */
  def getRubricaEvolution(): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("""
        SELECT fecha, rival, rubrica_posicion, rubrica_decisiones, rubrica_pies, rubrica_comunicacion, rubrica_actitud
        FROM matches
        WHERE status='PLAYED' AND rubrica_posicion IS NOT NULL
        ORDER BY fecha ASC""")
      var l = List[Map[String, Any]]()
      while (rs.next()) l = l :+ Map(
        "fecha" -> rs.getDate("fecha").toString, "rival" -> Option(rs.getString("rival")).getOrElse(""),
        "posicion" -> rs.getInt("rubrica_posicion"), "decisiones" -> rs.getInt("rubrica_decisiones"),
        "pies" -> rs.getInt("rubrica_pies"), "comunicacion" -> rs.getInt("rubrica_comunicacion"),
        "actitud" -> rs.getInt("rubrica_actitud")
      )
      l
    } finally { conn.close() }
  }

  /** Medias de rubrica de la temporada activa, para incluir en el contexto de getDeepAnalysis(). */
  def getRubricaMediasTemporada(): Option[Map[String, Double]] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(s"""
        SELECT AVG(rubrica_posicion) as posicion, AVG(rubrica_decisiones) as decisiones,
               AVG(rubrica_pies) as pies, AVG(rubrica_comunicacion) as comunicacion,
               AVG(rubrica_actitud) as actitud, COUNT(*) as n
        FROM matches
        WHERE status='PLAYED' AND rubrica_posicion IS NOT NULL ${seasonFilterActual()}""")
      if (rs.next() && rs.getInt("n") > 0) Some(Map(
        "posicion" -> rs.getDouble("posicion"), "decisiones" -> rs.getDouble("decisiones"),
        "pies" -> rs.getDouble("pies"), "comunicacion" -> rs.getDouble("comunicacion"),
        "actitud" -> rs.getDouble("actitud")
      )) else None
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE 4.4 — GUIA DE CONVERSACION POST-PARTIDO
  // ─────────────────────────────────────────────────────────────────────────────
  def generarGuiaConversacion(matchId: Int): Unit = {
    new Thread(() => {
      val conn = getConnection()
      try {
        val card = getLatestCardData(); val edad = calcularEdadExacta(card.fechaNacimiento)
        val ps = conn.prepareStatement("SELECT fecha, rival, goles_favor, goles_contra, nota, reaccion_goles, autopercepcion_prepartido FROM matches WHERE id=?")
        ps.setInt(1, matchId)
        val rs = ps.executeQuery()
        if (rs.next()) {
          val rival = rs.getString("rival"); val gf = rs.getInt("goles_favor"); val gc = rs.getInt("goles_contra")
          val nota = rs.getDouble("nota"); val comportamiento = Option(rs.getString("reaccion_goles")).getOrElse("N/A")

          // BLOQUE B3: autopercepcion pre-partido vs indice de forma calculado ese dia
          val autopercepcionObj = rs.getObject("autopercepcion_prepartido")
          val autopercepcionLinea = if (autopercepcionObj == null) "" else {
            val autop = rs.getInt("autopercepcion_prepartido")
            val fechaPartido = rs.getDate("fecha").toString
            val rsForma = conn.prepareStatement("SELECT indice_forma FROM forma_diaria WHERE fecha = ?::date")
            rsForma.setString(1, fechaPartido)
            val rf = rsForma.executeQuery()
            if (rf.next()) {
              val forma = rf.getDouble("indice_forma")
              val formaEn5 = math.min(5.0, math.max(1.0, forma / 2.0))
              val coincide = math.abs(autop - formaEn5) <= 1.0
              s" Antes del partido Héctor dijo que se encontraba $autop/5. El Índice de Forma calculado era ${"%.1f".format(forma)}. ${if (coincide) "Coinciden" else "Divergen significativamente"}."
            } else ""
          }

          val prompt = s"""Héctor, portero de $edad años, acaba de jugar contra $rival. Resultado: $gf-$gc. Nota: $nota. Comportamiento tras goles: $comportamiento.$autopercepcionLinea El padre va a comer con él ahora. Dame en texto plano: QUE_RESALTAR: [una acción positiva específica y concreta para mencionar con naturalidad en la comida] / QUE_CALLAR: [un error que debe evitar mencionar hoy, con una frase explicando por qué callarlo beneficia la resiliencia a esta edad] / ACCION_POSITIVA: [una micro-acción para esta tarde que refuerce la confianza, sin hablar de fútbol]. Lenguaje para un padre, no para un entrenador."""

          val guia = AIProvider.ask(prompt, None, bypassCache = true)
          val up = conn.prepareStatement("UPDATE matches SET guia_conversacion=? WHERE id=?")
          up.setString(1, guia); up.setInt(2, matchId)
          up.executeUpdate()
        }
      } catch { case e: Exception => println(s"[!] generarGuiaConversacion error: ${e.getMessage}") }
      finally { conn.close() }
    }).start()
  }

  def getGuiaConversacion(matchId: Int): Option[String] = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("SELECT guia_conversacion FROM matches WHERE id=?")
      ps.setInt(1, matchId)
      val rs = ps.executeQuery()
      if (rs.next()) Option(rs.getString("guia_conversacion")).filter(_.nonEmpty) else None
    } finally { conn.close() }
  }

  def getGoalsAnalysis(): Map[String, Any] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT mg.*, m.rival, m.fecha, m.goles_favor, m.goles_contra " +
        "FROM match_goals mg JOIN matches m ON mg.match_id = m.id ORDER BY m.fecha DESC, mg.minuto ASC")
      var rows = List[Map[String, String]]()
      while (rs.next()) {
        rows = rows :+ Map(
          "id"              -> rs.getInt("id").toString,
          "matchId"         -> rs.getInt("match_id").toString,
          "rival"           -> Option(rs.getString("rival")).getOrElse(""),
          "fecha"           -> Option(rs.getString("fecha")).getOrElse(""),
          "minuto"          -> rs.getInt("minuto").toString,
          "origen"          -> Option(rs.getString("origen")).getOrElse(""),
          "situacion"       -> Option(rs.getString("situacion")).getOrElse(""),
          "responsabilidad" -> Option(rs.getString("responsabilidad")).getOrElse(""),
          "eraParable"      -> Option(rs.getString("era_parable")).getOrElse(""),
          "zonaGol"         -> Option(rs.getString("zona_gol")).getOrElse(""),
          "notas"           -> Option(rs.getString("notas")).getOrElse(""),
          "resultado"       -> (rs.getInt("goles_favor").toString + "-" + rs.getInt("goles_contra").toString)
        )
      }

      val total: Int = rows.size
      // Agrupaciones
      def countBy(campo: String): Map[String, Int] =
        rows.groupBy(_(campo)).map { case (k,v) => k -> v.size }

      val porOrigen: Map[String, Int]         = countBy("origen")
      val porSituacion: Map[String, Int]       = countBy("situacion")
      val porResponsabilidad: Map[String, Int] = countBy("responsabilidad")
      val porParable: Map[String, Int]         = countBy("eraParable")

      // Goles evitables = responsabilidad Alta o Media + era_parable Si
      val evitables: Int   = rows.count(r => r("responsabilidad") == "Alta" || (r("responsabilidad") == "Media" && r("eraParable") == "Si"))
      val inevitables: Int = rows.count(r => r("responsabilidad") == "Ninguna")
      val dudosos: Int     = total - evitables - inevitables

      // Nota ajustada: de cada partido, descuenta los goles inevitables
      val rsNota = conn.createStatement().executeQuery(
        "SELECT m.id, m.nota, m.goles_contra, " +
        "COUNT(CASE WHEN mg.responsabilidad='Ninguna' THEN 1 END) as goles_defensa " +
        "FROM matches m LEFT JOIN match_goals mg ON mg.match_id = m.id " +
        "WHERE m.status='PLAYED' AND m.nota > 0 " +
        "GROUP BY m.id, m.nota, m.goles_contra ORDER BY m.fecha DESC LIMIT 20")
      var notaAjustadaTotal = 0.0; var notaAjustadaCount = 0
      var notaRealTotal = 0.0
      while (rsNota.next()) {
        val nota = rsNota.getDouble("nota")
        val gc   = rsNota.getInt("goles_contra")
        val golesDefensa = rsNota.getInt("goles_defensa")
        // Cada gol de defensa suma 0.5 pts a la nota ajustada (aprox)
        val notaAdj = math.min(10.0, nota + golesDefensa * 0.5)
        notaAjustadaTotal += notaAdj
        notaRealTotal += nota
        notaAjustadaCount += 1
      }
      val notaAjustada: Double = if (notaAjustadaCount > 0) notaAjustadaTotal / notaAjustadaCount else 0.0
      val notaReal: Double     = if (notaAjustadaCount > 0) notaRealTotal / notaAjustadaCount else 0.0

      Map(
        "total"            -> total,
        "evitables"        -> evitables,
        "inevitables"      -> inevitables,
        "dudosos"          -> dudosos,
        "porOrigen"        -> porOrigen,
        "porSituacion"     -> porSituacion,
        "porResponsabilidad" -> porResponsabilidad,
        "porParable"       -> porParable,
        "notaAjustada"     -> notaAjustada,
        "notaReal"         -> notaReal,
        "rows"             -> rows
      )
    } finally { conn.close() }
  }

  // == FASE 8: PSxG DELTA (Post-Shot xG vs Goals Conceded) =====================
  // BLOQUE B3: seasonId=0 = historico completo (comportamiento anterior, sin cambios)
  def getPSxGDeltaData(seasonId: Int = 0): Map[String, Any] = {
    val conn = getConnection()
    try {
      // Tabla de xG base por zona + situacion
      // Probabilidad de gol segun zona de porteria y tipo de disparo
      // Valores calibrados para futbol base (5-10 anos)
      def xGBase(zona: String, situacion: String): Double = {
        val xgZona: Double = zona match {
          case "MC" => 0.72  // centro medio — maxima probabilidad
          case "BC" => 0.65  // centro bajo
          case "TC" => 0.55  // centro alto
          case "ML" | "MR" => 0.42  // laterales medios
          case "BL" | "BR" => 0.38  // laterales bajos
          case "TL" | "TR" => 0.28  // esquinas
          case _           => 0.45  // sin zona — valor medio
        }
        val multSit: Double = situacion match {
          case s if s.contains("1v1")  => 1.35
          case s if s.contains("2v1")  => 1.20
          case s if s.contains("penalti") | s.contains("penalty") => 1.60
          case s if s.contains("libre") => 0.85
          case s if s.contains("cabeza") | s.contains("aereo") => 0.90
          case _ => 1.0
        }
        math.min(0.97, xgZona * multSit)
      }

      // Obtener todos los goles de match_goals con su zona y situacion
      val rsGoles = conn.createStatement().executeQuery(
        "SELECT mg.zona_gol, mg.situacion, mg.responsabilidad, mg.era_parable, " +
        "mg.minuto, m.fecha, m.rival, m.nota " +
        "FROM match_goals mg " +
        "JOIN matches m ON mg.match_id = m.id " +
        s"WHERE m.status = 'PLAYED' ${seasonFilter(seasonId)} " +
        "ORDER BY m.fecha DESC")

      case class GoalRow(zona: String, situacion: String, responsabilidad: String,
                         eraParable: String, minuto: Int, fecha: String,
                         rival: String, notaPartido: Double,
                         xg: Double)
      var goles = List[GoalRow]()
      while (rsGoles.next()) {
        val zona = Option(rsGoles.getString("zona_gol")).getOrElse("")
        val sit  = Option(rsGoles.getString("situacion")).getOrElse("").toLowerCase
        val xg   = xGBase(zona, sit)
        goles = goles :+ GoalRow(
          zona            = zona,
          situacion       = Option(rsGoles.getString("situacion")).getOrElse(""),
          responsabilidad = Option(rsGoles.getString("responsabilidad")).getOrElse(""),
          eraParable      = Option(rsGoles.getString("era_parable")).getOrElse(""),
          minuto          = rsGoles.getInt("minuto"),
          fecha           = Option(rsGoles.getString("fecha")).getOrElse("").take(10),
          rival           = Option(rsGoles.getString("rival")).getOrElse(""),
          notaPartido     = rsGoles.getDouble("nota"),
          xg              = xg
        )
      }

      val nGoles = goles.size
      // xG total acumulado (cuantos goles "debia" encajar estadisticamente)
      val xgTotal: Double = goles.map(_.xg).sum
      // Goles reales encajados
      val golesReales: Int = nGoles
      // PSxG Delta: goles reales - xG. Negativo = mejor que esperado
      val psxgDelta: Double = golesReales - xgTotal
      val psxgDeltaStr: String = (if (psxgDelta <= 0) "" else "+") + f"$psxgDelta%.2f"
      val psxgDeltaColor: String = if (psxgDelta <= -1.0) "success"
                                    else if (psxgDelta <= 0.5) "info"
                                    else if (psxgDelta <= 1.5) "warning"
                                    else "danger"
      val psxgLabel: String = if (psxgDelta <= -1.0) "BAJO LO ESPERADO"
                               else if (psxgDelta <= 0.5) "EN LO ESPERADO"
                               else if (psxgDelta <= 1.5) "ALGO POR ENCIMA"
                               else "POR ENCIMA"

      // Clasificacion de goles por dificultad del tiro
      val golesAltaDif   = goles.count(_.xg < 0.35)   // tiros muy dificiles
      val golesMediaDif  = goles.count(g => g.xg >= 0.35 && g.xg < 0.60)
      val golesBajaDif   = goles.count(_.xg >= 0.60)   // tiros faciles — los que mas duelen

      // Por zona: xG medio vs goles reales
      val zonas = List("TL","TC","TR","ML","MC","MR","BL","BC","BR")
      val porZona: List[Map[String, Any]] = zonas.map { z =>
        val golesZona = goles.filter(g => g.zona == z || (z == "" && g.zona.isEmpty))
        val nZ  = golesZona.size
        val xgZ = golesZona.map(_.xg).sum
        val deltaZ = nZ - xgZ
        Map(
          "zona"   -> z,
          "goles"  -> nZ,
          "xg"     -> xgZ,
          "delta"  -> deltaZ,
          "label"  -> (if (deltaZ <= -0.5) "Mejor" else if (deltaZ >= 0.5) "Peor" else "Normal")
        )
      }.filter(_("goles").asInstanceOf[Int] > 0)

      // Tabla individual de goles con xG
      val tablaGoles: List[Map[String, String]] = goles.take(20).map { g =>
        val deltaInd = 1.0 - g.xg  // 1 - xG = cuanto costó parar (inverso: para goles encajados, xG alto = facil)
        Map(
          "fecha"   -> g.fecha,
          "rival"   -> g.rival,
          "zona"    -> (if (g.zona.isEmpty) "—" else g.zona),
          "sit"     -> (if (g.situacion.isEmpty) "—" else g.situacion),
          "xg"      -> f"${g.xg}%.2f",
          "dific"   -> (if (g.xg < 0.35) "DIFÍCIL" else if (g.xg < 0.60) "MEDIA" else "FÁCIL"),
          "resp"    -> g.responsabilidad,
          "color"   -> (if (g.xg < 0.35) "success" else if (g.xg < 0.60) "warning" else "danger")
        )
      }

      // xG medio por partido
      val nPartidos = goles.map(_.fecha).distinct.size
      val xgPorPartido = if (nPartidos > 0) xgTotal / nPartidos else 0.0

      Map(
        "nGoles"         -> nGoles,
        "xgTotal"        -> xgTotal,
        "psxgDelta"      -> psxgDelta,
        "psxgDeltaStr"   -> psxgDeltaStr,
        "psxgDeltaColor" -> psxgDeltaColor,
        "psxgLabel"      -> psxgLabel,
        "golesAltaDif"   -> golesAltaDif,
        "golesMediaDif"  -> golesMediaDif,
        "golesBajaDif"   -> golesBajaDif,
        "xgPorPartido"   -> xgPorPartido,
        "nPartidos"      -> nPartidos,
        "porZona"        -> porZona,
        "tablaGoles"     -> tablaGoles
      )
    } finally { conn.close() }
  }

  // == FASE 8: COGNITIVE RESET RATE ============================================
  def getCognitiveResetData(): Map[String, Any] = {
    val conn = getConnection()
    try {
      // 1. Partidos con al menos un gol evitable (responsabilidad Alta o Media+parable)
      //    y la nota del partido SIGUIENTE
      val rs = conn.createStatement().executeQuery("""
        WITH partidos_con_error AS (
          SELECT DISTINCT ON (m.id)
            m.id, m.fecha, m.nota, m.rival,
            COUNT(mg.id) OVER (PARTITION BY m.id) as n_evitables
          FROM matches m
          JOIN match_goals mg ON mg.match_id = m.id
          WHERE m.status = 'PLAYED' AND m.nota > 0
            AND (mg.responsabilidad = 'Alta'
              OR (mg.responsabilidad = 'Media' AND mg.era_parable = 'Si'))
        ),
        partidos_ord AS (
          SELECT id, fecha, nota, rival,
            ROW_NUMBER() OVER (ORDER BY fecha ASC) AS rn
          FROM matches
          WHERE status = 'PLAYED' AND nota > 0
        )
        SELECT
          pe.id, pe.fecha, pe.nota AS nota_error, pe.rival,
          pe.n_evitables,
          nx.nota  AS nota_siguiente,
          nx.rival AS rival_siguiente,
          nx.fecha AS fecha_siguiente
        FROM partidos_con_error pe
        JOIN partidos_ord po ON po.id = pe.id
        JOIN partidos_ord nx ON nx.rn = po.rn + 1
        ORDER BY pe.fecha DESC
        LIMIT 20
      """)

      case class ResetRow(fecha: String, rival: String, notaError: Double,
                          nEvitables: Int, notaSig: Double, rivalSig: String,
                          fechaSig: String)
      var rows = List[ResetRow]()
      while (rs.next()) {
        rows = rows :+ ResetRow(
          fecha        = Option(rs.getString("fecha")).getOrElse(""),
          rival        = Option(rs.getString("rival")).getOrElse(""),
          notaError    = rs.getDouble("nota_error"),
          nEvitables   = rs.getInt("n_evitables"),
          notaSig      = rs.getDouble("nota_siguiente"),
          rivalSig     = Option(rs.getString("rival_siguiente")).getOrElse(""),
          fechaSig     = Option(rs.getString("fecha_siguiente")).getOrElse("")
        )
      }

      // 2. Metricas agregadas
      val n = rows.size
      val rebounds   = rows.count(r => r.notaSig >= r.notaError - 0.2)  // recuperó o mejoró
      val positivos  = rows.count(r => r.notaSig > r.notaError + 0.4)   // rebote claro
      val negativos  = rows.count(r => r.notaSig < r.notaError - 0.5)   // impacto negativo

      val resetScore: Int = if (n > 0) math.min(100, (rebounds.toDouble / n * 100).toInt) else 0
      val avgNotaError: Double = if (n > 0) rows.map(_.notaError).sum / n else 0.0
      val avgNotaSig: Double   = if (n > 0) rows.map(_.notaSig).sum / n else 0.0
      val avgDelta: Double     = avgNotaSig - avgNotaError

      // 3. Mediana de recuperacion (para ver si sube normalmente tras un error)
      val mediaGeneral: Double = {
        val rsM = conn.createStatement().executeQuery(
          "SELECT AVG(nota) FROM matches WHERE status='PLAYED' AND nota > 0")
        if (rsM.next()) rsM.getDouble(1) else 0.0
      }

      // 4. Clasificacion
      val clasificacion: String = if (resetScore >= 70) "RESILIENTE"
                                  else if (resetScore >= 45) "EN PROCESO"
                                  else "VULNERABLE"
      val clasificacionColor: String = if (resetScore >= 70) "success"
                                       else if (resetScore >= 45) "warning"
                                       else "danger"

      // 5. Series para grafico
      val fechasSerie: List[String] = rows.reverse.map(_.fechaSig.take(10))
      val notaErrorSerie: List[Double] = rows.reverse.map(_.notaError)
      val notaSigSerie: List[Double]   = rows.reverse.map(_.notaSig)

      // 6. Datos individuales para tabla
      val tablaRows: List[Map[String, String]] = rows.map { r =>
        val diff = r.notaSig - r.notaError
        val deltaStr = (if (diff >= 0) "+" else "") + f"$diff%.1f"
        val resultado = if (diff > 0.4) "REBOTE" else if (diff < -0.5) "IMPACTO" else "ESTABLE"
        Map(
          "fecha"      -> r.fecha.take(10),
          "rival"      -> r.rival,
          "notaError"  -> f"${r.notaError}%.1f",
          "nEvitables" -> r.nEvitables.toString,
          "fechaSig"   -> r.fechaSig.take(10),
          "rivalSig"   -> r.rivalSig,
          "notaSig"    -> f"${r.notaSig}%.1f",
          "delta"      -> deltaStr,
          "resultado"  -> resultado
        )
      }

      Map(
        "n"                -> n,
        "resetScore"       -> resetScore,
        "clasificacion"    -> clasificacion,
        "clasificacionColor" -> clasificacionColor,
        "rebounds"         -> rebounds,
        "positivos"        -> positivos,
        "negativos"        -> negativos,
        "avgNotaError"     -> avgNotaError,
        "avgNotaSig"       -> avgNotaSig,
        "avgDelta"         -> avgDelta,
        "mediaGeneral"     -> mediaGeneral,
        "fechasSerie"      -> fechasSerie,
        "notaErrorSerie"   -> notaErrorSerie,
        "notaSigSerie"     -> notaSigSerie,
        "tablaRows"        -> tablaRows
      )
    } finally { conn.close() }
  }


  // == FASE 8: DEVELOPMENT PATHWAY MATCHER =====================================
  def getPathwayData(): Map[String, Any] = {
    val conn = getConnection()
    try {
      // Cruza el arquetipo de rivales (Striker Clustering) con métricas de Héctor
      // Responde: ¿contra qué estilo de equipo crece más como portero?

      val rs = conn.createStatement().executeQuery("""
        SELECT
          m.rival,
          m.nota,
          m.paradas,
          m.goles_contra,
          m.acciones_pie,
          m.lineas_superadas,
          m.scanning_rate,
          COUNT(mg.id)                                                       AS n_goles,
          COUNT(CASE WHEN mg.situacion ILIKE '%1v1%' THEN 1 END)            AS g_1v1,
          COUNT(CASE WHEN mg.situacion ILIKE '%aereo%'
                       OR mg.situacion ILIKE '%cabeza%' THEN 1 END)         AS g_aereo,
          COUNT(CASE WHEN mg.situacion ILIKE '%2v1%' THEN 1 END)            AS g_2v1
        FROM matches m
        LEFT JOIN match_goals mg ON mg.match_id = m.id
        WHERE m.status = 'PLAYED' AND m.nota > 0
        GROUP BY m.id, m.rival, m.nota, m.paradas, m.goles_contra,
                 m.acciones_pie, m.lineas_superadas, m.scanning_rate
        ORDER BY m.fecha DESC
      """)

      case class PRow(rival: String, nota: Double, paradas: Int, gc: Int,
                      pie: Int, lineas: Int, scan: Int,
                      nGoles: Int, g1v1: Int, gAereo: Int, g2v1: Int)
      var rows = List[PRow]()
      while (rs.next()) {
        rows = rows :+ PRow(
          rival   = Option(rs.getString("rival")).getOrElse(""),
          nota    = rs.getDouble("nota"),
          paradas = rs.getInt("paradas"),
          gc      = rs.getInt("goles_contra"),
          pie     = rs.getInt("acciones_pie"),
          lineas  = rs.getInt("lineas_superadas"),
          scan    = rs.getInt("scanning_rate"),
          nGoles  = rs.getInt("n_goles"),
          g1v1    = rs.getInt("g_1v1"),
          gAereo  = rs.getInt("g_aereo"),
          g2v1    = rs.getInt("g_2v1")
        )
      }

      // Clasificar cada partido por arquetipo de rival (misma lógica que Striker Clustering)
      def arquetipoRival(r: PRow): String = {
        val nG = r.nGoles
        if (r.g1v1 >= 2 || (nG > 0 && r.g1v1.toDouble/nG >= 0.35)) "RAPIDO"
        else if (r.gAereo >= 2 || (nG > 0 && r.gAereo.toDouble/nG >= 0.30)) "AEREO"
        else if (r.g2v1 >= 2 || (nG > 0 && r.g2v1.toDouble/nG >= 0.30)) "COLECTIVO"
        else if (r.gc >= 3) "DIRECTO"
        else "EQUILIBRADO"
      }

      // Agrupar por arquetipo y calcular métricas de crecimiento
      case class PathStats(nota: Double, paradas: Double, pie: Double,
                           lineas: Double, n: Int)

      def statsFor(arq: String): Option[PathStats] = {
        val sub = rows.filter(r => arquetipoRival(r) == arq)
        if (sub.isEmpty) None
        else Some(PathStats(
          nota    = sub.map(_.nota).sum / sub.size,
          paradas = sub.map(_.paradas.toDouble).sum / sub.size,
          pie     = sub.map(_.pie.toDouble).sum / sub.size,
          lineas  = sub.map(_.lineas.toDouble).sum / sub.size,
          n       = sub.size
        ))
      }

      val arqs = List("RAPIDO","AEREO","COLECTIVO","DIRECTO","EQUILIBRADO")
      val perArq: List[Map[String, Any]] = arqs.flatMap { arq =>
        statsFor(arq).map { s =>
          Map(
            "arquetipo" -> arq,
            "n"         -> s.n,
            "nota"      -> s.nota,
            "paradas"   -> s.paradas,
            "pie"       -> s.pie,
            "lineas"    -> s.lineas,
            "color"     -> (arq match {
              case "RAPIDO"     => "danger"
              case "AEREO"      => "info"
              case "COLECTIVO"  => "warning"
              case "DIRECTO"    => "primary"
              case _            => "secondary"
            })
          )
        }
      }

      // Mejor arquetipo para crecer (nota más alta = entorno más favorable)
      val mejorArq = if (perArq.nonEmpty)
        perArq.maxBy(_("nota").asInstanceOf[Double])
      else Map("arquetipo" -> "—", "nota" -> 0.0, "color" -> "secondary")

      // Arquetipo más desafiante (nota más baja = área de mejora)
      val peorArq = if (perArq.nonEmpty)
        perArq.minBy(_("nota").asInstanceOf[Double])
      else Map("arquetipo" -> "—", "nota" -> 0.0, "color" -> "secondary")

      // Recomendacion de entorno de desarrollo
      val recomendacion: String = mejorArq("arquetipo").asInstanceOf[String] match {
        case "RAPIDO"     => "Héctor rinde mejor contra equipos rápidos. Busca rivales con pressing alto y 1v1 frecuentes para consolidar esta fortaleza."
        case "AEREO"      => "Su mejor rendimiento es contra equipos aéreos. Los entrenamientos de salida en córner y dominio del área deben ser prioritarios."
        case "COLECTIVO"  => "Rinde bien en entornos de juego colectivo. Equipos con buen juego combinativo le sacan el máximo partido."
        case "DIRECTO"    => "Mejora contra el juego directo. Equipos que juegan largo le entrenan la salida y el despeje."
        case _            => "Perfil equilibrado. Cualquier estilo de rival le aporta crecimiento similar."
      }

      val areasMejora: String = peorArq("arquetipo").asInstanceOf[String] match {
        case "RAPIDO"     => "Trabajar salida en 1v1 y achique de ángulo con presión temporal."
        case "AEREO"      => "Reforzar dominio del área aérea — posición, grito y timing de salida."
        case "COLECTIVO"  => "Mejorar lectura de jugadas 2v1 y anticipación del pase de gol."
        case "DIRECTO"    => "Consolidar la salida a balones largos y la comunicación con la defensa."
        case _            => "Mantener la consistencia independientemente del estilo rival."
      }

      Map(
        "perArq"        -> perArq,
        "mejorArq"      -> mejorArq,
        "peorArq"       -> peorArq,
        "recomendacion" -> recomendacion,
        "areasMejora"   -> areasMejora,
        "totalPartidos" -> rows.size
      )
    } finally { conn.close() }
  }

  // == FASE 5: BIO-BANDING =====================================================
  def getBioBandingData(): Map[String, Any] = {
    val conn = getConnection()
    try {
      // 1. Edad y PHV del Digital Twin
      val rsP = conn.createStatement().executeQuery(
        "SELECT fecha_nacimiento FROM seasons ORDER BY id DESC LIMIT 1")
      val fechaNac = if (rsP.next())
        Option(rsP.getDate("fecha_nacimiento")).map(_.toString).getOrElse("2015-06-19")
      else "2015-06-19"
      val hoy       = java.time.LocalDate.now()
      val nac       = java.time.LocalDate.parse(fechaNac)
      val edadAnios = java.time.Period.between(nac, hoy).getYears
      val edadMeses = java.time.Period.between(nac, hoy).getYears * 12 +
                      java.time.Period.between(nac, hoy).getMonths

      // 2. Altura actual y velocidad de crecimiento
      val rsG = conn.createStatement().executeQuery(
        "SELECT altura, peso, velocidad_crecimiento FROM physical_growth ORDER BY fecha DESC LIMIT 1")
      val (alturaActual, pesoActual, velCrecimiento) =
        if (rsG.next()) (rsG.getDouble("altura"), rsG.getDouble("peso"),
                         rsG.getDouble("velocidad_crecimiento"))
        else (0.0, 0.0, 0.0)

      // 3. Velocidad máxima (PHV detector)
      val rsVel = conn.createStatement().executeQuery(
        "SELECT MAX(velocidad_crecimiento) as max_vel FROM physical_growth")
      val phvVelocidad = if (rsVel.next()) rsVel.getDouble("max_vel") else 0.0
      val phvActivo    = phvVelocidad >= 6.0  // >6 cm/año = pico activo

      // 4. Fase biológica estimada
      val faseBio: String = if (edadAnios < 8) "INFANCIA TARDÍA"
                             else if (edadAnios < 10) "PRE-PUBERTAD"
                             else if (edadAnios < 12) "INICIO PUBERTAD"
                             else if (phvActivo) "PHV — PICO ACTIVO"
                             else if (edadAnios < 15) "PUBERTAD MEDIA"
                             else if (edadAnios < 17) "POST-PHV"
                             else "MADUREZ"
      val faseBioColor: String = faseBio match {
        case "PHV — PICO ACTIVO" => "danger"
        case "INICIO PUBERTAD"   => "warning"
        case "PUBERTAD MEDIA"    => "warning"
        case "POST-PHV"          => "info"
        case _                   => "secondary"
      }

      // 5. Factor de ajuste de nota según fase biológica
      // Durante el PHV el cuerpo consume energía en crecer — rendimiento esperado baja
      // Un 6.5 durante PHV activo equivale a un 7.5 en condiciones normales
      val factorAjuste: Double = faseBio match {
        case "PHV — PICO ACTIVO" => 1.15   // +15%: notas más valiosas durante el pico
        case "INICIO PUBERTAD"   => 1.08   // +8%
        case "PUBERTAD MEDIA"    => 1.05   // +5%
        case "PRE-PUBERTAD"      => 1.02   // +2%: ajuste mínimo
        case _                   => 1.0    // sin ajuste
      }

      // 6. Nota real vs nota bio-ajustada (últimos 20 partidos)
      val rsM = conn.createStatement().executeQuery(
        "SELECT fecha, rival, nota, goles_contra, paradas " +
        "FROM matches WHERE status='PLAYED' AND nota > 0 " +
        "ORDER BY fecha DESC LIMIT 20")
      var matchRows = List[Map[String, Any]]()
      while (rsM.next()) {
        val nota      = rsM.getDouble("nota")
        val notaAdj   = math.min(10.0, nota * factorAjuste)
        matchRows = matchRows :+ Map(
          "fecha"    -> rsM.getString("fecha").take(10),
          "rival"    -> Option(rsM.getString("rival")).getOrElse(""),
          "nota"     -> nota,
          "notaAdj"  -> notaAdj,
          "gc"       -> rsM.getInt("goles_contra"),
          "paradas"  -> rsM.getInt("paradas")
        )
      }

      val n = matchRows.size
      val avgNota    = if (n > 0) matchRows.map(_("nota").asInstanceOf[Double]).sum / n else 0.0
      val avgNotaAdj = if (n > 0) matchRows.map(_("notaAdj").asInstanceOf[Double]).sum / n else 0.0
      val deltaMedia = avgNotaAdj - avgNota

      // 7. Percentil de altura para la edad (estimación simplificada OMS)
      val percentilAltura: String = if (alturaActual <= 0) "Sin datos" else {
        // Medianas OMS para niños (cm) por edad
        val medianas = Map(5->109.0, 6->116.0, 7->122.0, 8->128.0, 9->133.0,
                           10->138.0, 11->143.0, 12->149.0, 13->156.0, 14->163.0,
                           15->169.0, 16->173.0, 17->175.0, 18->176.0)
        val mediana = medianas.getOrElse(edadAnios, 155.0)
        val diff = alturaActual - mediana
        if (diff > 6) "P97 — Muy alto para su edad"
        else if (diff > 3) "P75-P90 — Alto para su edad"
        else if (diff > -3) "P50 — Talla media"
        else if (diff > -6) "P25 — Algo por debajo"
        else "P10 — Por debajo de la media"
      }

      // 8. Series para gráfico
      val fechasSerie  = matchRows.reverse.map(_("fecha").asInstanceOf[String].take(5))
      val notaSerie    = matchRows.reverse.map(_("nota").asInstanceOf[Double])
      val notaAdjSerie = matchRows.reverse.map(_("notaAdj").asInstanceOf[Double])

      Map(
        "edadAnios"       -> edadAnios,
        "edadMeses"       -> edadMeses,
        "alturaActual"    -> alturaActual,
        "pesoActual"      -> pesoActual,
        "velCrecimiento"  -> velCrecimiento,
        "phvActivo"       -> phvActivo,
        "phvVelocidad"    -> phvVelocidad,
        "faseBio"         -> faseBio,
        "faseBioColor"    -> faseBioColor,
        "factorAjuste"    -> factorAjuste,
        "avgNota"         -> avgNota,
        "avgNotaAdj"      -> avgNotaAdj,
        "deltaMedia"      -> deltaMedia,
        "percentilAltura" -> percentilAltura,
        "matchRows"       -> matchRows,
        "fechasSerie"     -> fechasSerie,
        "notaSerie"       -> notaSerie,
        "notaAdjSerie"    -> notaAdjSerie,
        "n"               -> n
      )
    } finally { conn.close() }
  }

  // == FASE 7: STRIKER CLUSTERING ==============================================
  // BLOQUE B3: seasonId=0 = historico completo (comportamiento anterior, sin cambios)
  def getStrikerClusters(seasonId: Int = 0): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      // Agrupamos rivales por perfil de ataque usando datos ya disponibles
      // Arquetipo: RAPIDO (muchos goles en contraataque/1v1), FISICO (muchos goles aereos/2v1),
      //            TECNICO (pocos goles pero alta nota rival), DIRECTO (muchos goles de tiro lejano)
      val rs = conn.createStatement().executeQuery(s"""
        SELECT
          m.rival,
          COUNT(DISTINCT m.id)                                              AS pj,
          SUM(m.goles_contra)                                               AS gc_total,
          AVG(m.goles_contra)                                               AS gc_media,
          AVG(m.nota)                                                       AS nota_media_hector,
          COUNT(mg.id)                                                      AS goles_analizados,
          COUNT(CASE WHEN mg.situacion ILIKE '%1v1%' THEN 1 END)           AS g_1v1,
          COUNT(CASE WHEN mg.situacion ILIKE '%2v1%' THEN 1 END)           AS g_2v1,
          COUNT(CASE WHEN mg.situacion ILIKE '%aereo%'
                       OR mg.situacion ILIKE '%cabeza%' THEN 1 END)        AS g_aereo,
          COUNT(CASE WHEN mg.origen ILIKE '%contra%'
                       OR mg.minuto <= 15
                       OR mg.minuto >= 75 THEN 1 END)                      AS g_presion,
          MAX(m.goles_contra)                                               AS gc_max
        FROM matches m
        LEFT JOIN match_goals mg ON mg.match_id = m.id
        WHERE m.status = 'PLAYED' ${seasonFilter(seasonId)}
        GROUP BY m.rival
        HAVING COUNT(DISTINCT m.id) >= 1
        ORDER BY gc_total DESC
        LIMIT 30
      """)

      var clusters = List[Map[String, Any]]()
      while (rs.next()) {
        val rival    = Option(rs.getString("rival")).getOrElse("")
        val pj       = rs.getInt("pj")
        val gcTotal  = rs.getInt("gc_total")
        val gcMedia  = rs.getDouble("gc_media")
        val notaHec  = rs.getDouble("nota_media_hector")
        val gAnz     = rs.getInt("goles_analizados")
        val g1v1     = rs.getInt("g_1v1")
        val g2v1     = rs.getInt("g_2v1")
        val gAereo   = rs.getInt("g_aereo")
        val gPresion = rs.getInt("g_presion")
        val gcMax    = rs.getInt("gc_max")

        // Clasificacion por arquetipo
        val arquetipo: String = if (g1v1 >= 2 || (gAnz > 0 && g1v1.toDouble / gAnz >= 0.35))
          "RAPIDO"
        else if (gAereo >= 2 || (gAnz > 0 && gAereo.toDouble / gAnz >= 0.30))
          "AEREO"
        else if (g2v1 >= 2 || (gAnz > 0 && g2v1.toDouble / gAnz >= 0.30))
          "COLECTIVO"
        else if (gcMedia >= 2.5)
          "DIRECTO"
        else
          "EQUILIBRADO"

        val arquetipoColor: String = arquetipo match {
          case "RAPIDO"     => "danger"
          case "AEREO"      => "info"
          case "COLECTIVO"  => "warning"
          case "DIRECTO"    => "primary"
          case _            => "secondary"
        }

        val amenaza: String = if (gcMedia >= 3.0) "ALTA"
                               else if (gcMedia >= 1.5) "MEDIA"
                               else "BAJA"
        val amenazaColor: String = if (gcMedia >= 3.0) "danger"
                                    else if (gcMedia >= 1.5) "warning"
                                    else "success"

        clusters = clusters :+ Map(
          "rival"          -> rival,
          "pj"             -> pj,
          "gcTotal"        -> gcTotal,
          "gcMedia"        -> gcMedia,
          "notaHec"        -> notaHec,
          "g1v1"           -> g1v1,
          "g2v1"           -> g2v1,
          "gAereo"         -> gAereo,
          "gPresion"       -> gPresion,
          "gcMax"          -> gcMax,
          "arquetipo"      -> arquetipo,
          "arquetipoColor" -> arquetipoColor,
          "amenaza"        -> amenaza,
          "amenazaColor"   -> amenazaColor
        )
      }
      clusters
    } finally { conn.close() }
  }

  // == FASE 7: RED-ZONE ANALYTICS ==============================================
  // BLOQUE B3: seasonId=0 = historico completo (comportamiento anterior, sin cambios)
  def getRedZoneData(seasonId: Int = 0): Map[String, Any] = {
    val conn = getConnection()
    try {
      val sf = seasonFilter(seasonId)
      // 1. Media global de referencia
      val rsGlobal = conn.createStatement().executeQuery(
        "SELECT AVG(nota) as avg_nota, AVG(paradas) as avg_paradas, " +
        "AVG(goles_contra) as avg_gc, COUNT(*) as total " +
        s"FROM matches WHERE status='PLAYED' AND nota > 0 $sf")
      val (avgNotaGlobal, avgParadasGlobal, avgGcGlobal, totalPartidos) =
        if (rsGlobal.next()) (rsGlobal.getDouble("avg_nota"), rsGlobal.getDouble("avg_paradas"),
                              rsGlobal.getDouble("avg_gc"),   rsGlobal.getInt("total"))
        else (0.0, 0.0, 0.0, 0)

      // 2. Partidos de alta presion: goles_contra >= 2 (asedio ofensivo)
      val rsAsedio = conn.createStatement().executeQuery(
        "SELECT id, fecha, rival, nota, paradas, goles_contra, goles_favor, minutos " +
        s"FROM matches WHERE status='PLAYED' AND nota > 0 AND goles_contra >= 2 $sf " +
        "ORDER BY fecha DESC LIMIT 30")
      var asedioRows = List[Map[String, Any]]()
      while (rsAsedio.next()) {
        asedioRows = asedioRows :+ Map(
          "fecha"  -> rsAsedio.getString("fecha").take(10),
          "rival"  -> Option(rsAsedio.getString("rival")).getOrElse(""),
          "nota"   -> rsAsedio.getDouble("nota"),
          "paradas"-> rsAsedio.getInt("paradas"),
          "gc"     -> rsAsedio.getInt("goles_contra"),
          "gf"     -> rsAsedio.getInt("goles_favor"),
          "minutos"-> rsAsedio.getInt("minutos")
        )
      }
      val nAsedio = asedioRows.size
      val avgNotaAsedio   = if (nAsedio > 0) asedioRows.map(_(("nota")).asInstanceOf[Double]).sum / nAsedio else 0.0
      val avgParadasAsedio= if (nAsedio > 0) asedioRows.map(_(("paradas")).asInstanceOf[Int].toDouble).sum / nAsedio else 0.0

      // 3. Partidos disputados con minutos >= 70 (final del partido - zona de fatiga)
      val rsFatiga = conn.createStatement().executeQuery(
        "SELECT id, fecha, rival, nota, paradas, goles_contra, goles_favor, minutos " +
        s"FROM matches WHERE status='PLAYED' AND nota > 0 AND minutos >= 70 $sf " +
        "ORDER BY fecha DESC LIMIT 30")
      var fatigaRows = List[Map[String, Any]]()
      while (rsFatiga.next()) {
        fatigaRows = fatigaRows :+ Map(
          "fecha"  -> rsFatiga.getString("fecha").take(10),
          "rival"  -> Option(rsFatiga.getString("rival")).getOrElse(""),
          "nota"   -> rsFatiga.getDouble("nota"),
          "paradas"-> rsFatiga.getInt("paradas"),
          "gc"     -> rsFatiga.getInt("goles_contra"),
          "gf"     -> rsFatiga.getInt("goles_favor"),
          "minutos"-> rsFatiga.getInt("minutos")
        )
      }
      val nFatiga = fatigaRows.size
      val avgNotaFatiga    = if (nFatiga > 0) fatigaRows.map(_(("nota")).asInstanceOf[Double]).sum / nFatiga else 0.0
      val avgParadasFatiga = if (nFatiga > 0) fatigaRows.map(_(("paradas")).asInstanceOf[Int].toDouble).sum / nFatiga else 0.0

      // 4. Partidos de derrota abultada (gc >= 3) — colapso total
      val rsColapso = conn.createStatement().executeQuery(
        "SELECT id, fecha, rival, nota, paradas, goles_contra, goles_favor " +
        s"FROM matches WHERE status='PLAYED' AND nota > 0 AND goles_contra >= 3 $sf " +
        "ORDER BY fecha DESC LIMIT 20")
      var colapsoRows = List[Map[String, Any]]()
      while (rsColapso.next()) {
        colapsoRows = colapsoRows :+ Map(
          "fecha"  -> rsColapso.getString("fecha").take(10),
          "rival"  -> Option(rsColapso.getString("rival")).getOrElse(""),
          "nota"   -> rsColapso.getDouble("nota"),
          "paradas"-> rsColapso.getInt("paradas"),
          "gc"     -> rsColapso.getInt("goles_contra"),
          "gf"     -> rsColapso.getInt("goles_favor")
        )
      }
      val nColapso = colapsoRows.size
      val avgNotaColapso = if (nColapso > 0) colapsoRows.map(_(("nota")).asInstanceOf[Double]).sum / nColapso else 0.0

      // 5. Resilience Index: nota en asedio vs nota global (0-100)
      val resilienceIndex: Int = if (avgNotaGlobal > 0)
        math.min(100, math.max(0, ((avgNotaAsedio / avgNotaGlobal) * 100).toInt))
        else 0
      val resilienceLabel: String = if (resilienceIndex >= 90) "ÉLITE"
                                    else if (resilienceIndex >= 75) "SOLIDO"
                                    else if (resilienceIndex >= 55) "EN PROCESO"
                                    else "VULNERABLE"
      val resilienceColor: String = if (resilienceIndex >= 90) "warning"
                                    else if (resilienceIndex >= 75) "success"
                                    else if (resilienceIndex >= 55) "info"
                                    else "danger"

      // 6. Fatigue Index: nota en partidos largos vs global
      val fatigueIndex: Int = if (avgNotaGlobal > 0)
        math.min(100, math.max(0, ((avgNotaFatiga / avgNotaGlobal) * 100).toInt))
        else 0
      val fatigueLabel: String = if (fatigueIndex >= 90) "SIN CAIDA"
                                  else if (fatigueIndex >= 75) "AGUANTA"
                                  else if (fatigueIndex >= 55) "LEVE CAIDA"
                                  else "FATIGA CLARA"
      val fatigueColor: String = if (fatigueIndex >= 90) "success"
                                  else if (fatigueIndex >= 75) "info"
                                  else if (fatigueIndex >= 55) "warning"
                                  else "danger"

      // 7. Serie temporal: nota en asedio (ultimos 15)
      val asedioSerie   = asedioRows.reverse.takeRight(15).map(_("nota").asInstanceOf[Double])
      val asedioLabels  = asedioRows.reverse.takeRight(15).map(_("fecha").asInstanceOf[String].take(5))
      val globalLine    = asedioSerie.map(_ => avgNotaGlobal)

      Map(
        "totalPartidos"     -> totalPartidos,
        "avgNotaGlobal"     -> avgNotaGlobal,
        "avgParadasGlobal"  -> avgParadasGlobal,
        "avgGcGlobal"       -> avgGcGlobal,
        // Asedio
        "nAsedio"           -> nAsedio,
        "avgNotaAsedio"     -> avgNotaAsedio,
        "avgParadasAsedio"  -> avgParadasAsedio,
        "resilienceIndex"   -> resilienceIndex,
        "resilienceLabel"   -> resilienceLabel,
        "resilienceColor"   -> resilienceColor,
        // Fatiga
        "nFatiga"           -> nFatiga,
        "avgNotaFatiga"     -> avgNotaFatiga,
        "avgParadasFatiga"  -> avgParadasFatiga,
        "fatigueIndex"      -> fatigueIndex,
        "fatigueLabel"      -> fatigueLabel,
        "fatigueColor"      -> fatigueColor,
        // Colapso
        "nColapso"          -> nColapso,
        "avgNotaColapso"    -> avgNotaColapso,
        // Series
        "asedioSerie"       -> asedioSerie,
        "asedioLabels"      -> asedioLabels,
        "globalLine"        -> globalLine,
        // Tablas detalle
        "asedioRows"        -> asedioRows,
        "fatigaRows"        -> fatigaRows
      )
    } finally { conn.close() }
  }

  // == FASE 6.5: MONEYBALL & DEEP INFLUENCE ANALYTICS =========================
  def getMoneyballData(): Map[String, Any] = {
    val conn = getConnection()
    try {
      // ── 1. xT_GK: Expected Threat de Distribucion ──────────────────────────
      // Mide el peligro generado con el pie. Formula:
      // xT = (exito_pases_cortos * 0.4 + exito_pases_largos * 0.6) * volumen_relativo
      val rsXT = conn.createStatement().executeQuery(
        "SELECT pc_t, pc_ok, pl_t, pl_ok, acciones_pie, nota " +
        "FROM matches WHERE status='PLAYED' AND (pc_t+pl_t) > 0 ORDER BY fecha DESC LIMIT 30")
      var xtRows = List[(Int,Int,Int,Int,Int,Double)]()
      while (rsXT.next()) xtRows = xtRows :+ (
        rsXT.getInt("pc_t"), rsXT.getInt("pc_ok"),
        rsXT.getInt("pl_t"), rsXT.getInt("pl_ok"),
        rsXT.getInt("acciones_pie"), rsXT.getDouble("nota"))

      val xtPerMatch: List[Double] = xtRows.map { case (pct,pco,plt,plo,pie,_) =>
        val eficCorto: Double = if (pct > 0) pco.toDouble / pct else 0.0
        val eficLargo: Double = if (plt > 0) plo.toDouble / plt else 0.0
        val eficPond: Double  = eficCorto * 0.4 + eficLargo * 0.6
        val volumen: Double   = math.min(1.0, (pct + plt).toDouble / 20.0)
        eficPond * volumen * pie.toDouble * 10.0
      }
      val xtScore: Double = if (xtPerMatch.nonEmpty) xtPerMatch.sum / xtPerMatch.size else 0.0

      // Correlacion xT -> nota
      val xtNotas: List[(Double,Double)] = xtRows.zip(xtPerMatch).map { case ((a,b,c,d,e,nota),xt) => (xt,nota) }
      val xtCorr: Double = calcCorrelation(xtNotas)

      // Serie temporal para grafico (ultimos 20)
      val xtSerie: List[Double] = xtPerMatch.takeRight(20)

      // ── 2. xPoints: Expected Points Saved (Clutch Factor) ──────────────────
      // Valor de las paradas ponderado por: importancia del partido x tension del marcador
      // Importancia: TORNEO=2.0, LIGA=1.0, AMISTOSO=0.5
      // Tension: paradas en partidos ajustados (diferencia goles <= 1) valen mas
      val rsXP = conn.createStatement().executeQuery(
        "SELECT paradas, paradas_1v1, nota, goles_favor, goles_contra, tipo_partido, torneo_nombre " +
        "FROM matches WHERE status='PLAYED' AND paradas > 0 ORDER BY fecha DESC LIMIT 30")
      var xpRows = List[(Int,Int,Double,Int,Int,String,String)]()
      while (rsXP.next()) xpRows = xpRows :+ (
        rsXP.getInt("paradas"), rsXP.getInt("paradas_1v1"),
        rsXP.getDouble("nota"), rsXP.getInt("goles_favor"),
        rsXP.getInt("goles_contra"), Option(rsXP.getString("tipo_partido")).getOrElse("LIGA"),
        Option(rsXP.getString("torneo_nombre")).getOrElse(""))

      val xpPerMatch: List[Double] = xpRows.map { case (par, par1v1, nota, gf, gc, tipo, torneo) =>
        val importancia: Double = if (torneo.nonEmpty && tipo == "TORNEO") 2.0
                                  else if (tipo == "LIGA") 1.0 else 0.5
        val diferencia: Int    = math.abs(gf - gc)
        val tension: Double    = if (diferencia == 0) 1.5 else if (diferencia == 1) 1.2 else 1.0
        val valorParadas: Double = par * 1.0 + par1v1 * 0.8  // 1v1 son de alto valor
        valorParadas * importancia * tension * (nota / 100.0)
      }
      val xpTotal: Double   = if (xpPerMatch.nonEmpty) xpPerMatch.sum else 0.0
      val xpMedia: Double   = if (xpPerMatch.nonEmpty) xpTotal / xpPerMatch.size else 0.0
      val xpSerie: List[Double] = xpPerMatch.takeRight(20)

      // Clutch rating 0-100
      val maxXP: Double = if (xpPerMatch.nonEmpty) xpPerMatch.max else 1.0
      val clutchRating: Int = math.min(100, (xpMedia / math.max(maxXP, 1.0) * 100).toInt)

      // ── 3. SPV: Sweeper Keeper / Shot Prevention Value ──────────────────────
      // Cuantifica el valor del portero mas alla de la porteria
      // Paradas 1v1 (alto riesgo) + aereas (dominio espacio) + normal
      val rsSPV = conn.createStatement().executeQuery(
        "SELECT paradas, paradas_1v1, paradas_aereas, nota, fecha " +
        "FROM matches WHERE status='PLAYED' ORDER BY fecha DESC LIMIT 30")
      var spvRows = List[(Int,Int,Int,Double)]()
      while (rsSPV.next()) spvRows = spvRows :+ (
        rsSPV.getInt("paradas"), rsSPV.getInt("paradas_1v1"),
        rsSPV.getInt("paradas_aereas"), rsSPV.getDouble("nota"))

      val spvPerMatch: List[Double] = spvRows.map { case (par, p1v1, paer, nota) =>
        p1v1 * 1.5 + paer * 1.2 + math.max(0, par - p1v1 - paer) * 1.0
      }
      val spvMedia: Double   = if (spvPerMatch.nonEmpty) spvPerMatch.sum / spvPerMatch.size else 0.0
      val spvMax: Double     = if (spvPerMatch.nonEmpty) spvPerMatch.max else 1.0
      val spvScore: Int      = math.min(100, (spvMedia / math.max(spvMax, 1.0) * 100 * 1.5).toInt)
      val spvSerie: List[Double] = spvPerMatch.takeRight(20)

      // Breakdown por tipo
      val spv1v1Total: Int  = spvRows.map(_._2).sum
      val spvAerTotal: Int  = spvRows.map(_._3).sum
      val spvNorTotal: Int  = spvRows.map(r => math.max(0, r._1 - r._2 - r._3)).sum
      val spvTotalAll: Int  = spv1v1Total + spvAerTotal + spvNorTotal
      val pct1v1: Int       = if (spvTotalAll > 0) (spv1v1Total * 100 / spvTotalAll) else 0
      val pctAer: Int       = if (spvTotalAll > 0) (spvAerTotal * 100 / spvTotalAll) else 0
      val pctNor: Int       = if (spvTotalAll > 0) (spvNorTotal * 100 / spvTotalAll) else 0

      // ── 4. Bypass Rate: Lineas Superadas ────────────────────────────────────
      val rsBP = conn.createStatement().executeQuery(
        "SELECT lineas_superadas, acciones_pie, fecha FROM matches " +
        "WHERE status='PLAYED' ORDER BY fecha DESC LIMIT 30")
      var bpRows = List[(Int,Int)]()
      while (rsBP.next()) bpRows = bpRows :+ (rsBP.getInt("lineas_superadas"), rsBP.getInt("acciones_pie"))
      val bpMedia: Double   = if (bpRows.nonEmpty) bpRows.map(_._1.toDouble).sum / bpRows.size else 0.0
      val bpConDatos: Int   = bpRows.count(_._1 > 0)
      val bpEfic: Double    = {
        val filas = bpRows.filter(_._2 > 0)
        if (filas.nonEmpty) filas.map(r => r._1.toDouble / r._2).sum / filas.size else 0.0
      }
      val bpSerie: List[Int] = bpRows.map(_._1).takeRight(20)

      // ── 5. ROI de Entrenamiento ──────────────────────────────────────────────
      // Para cada partido, busca los entrenamientos en los 7 dias previos
      // y calcula la correlacion entre calidad/atencion media y la nota del partido
      val rsROI = conn.createStatement().executeQuery(
        "SELECT m.fecha as mfecha, m.nota, " +
        "  (SELECT AVG(t.calidad) FROM trainings t WHERE t.fecha BETWEEN m.fecha - INTERVAL '7 days' AND m.fecha) as avg_calidad, " +
        "  (SELECT AVG(t.atencion) FROM trainings t WHERE t.fecha BETWEEN m.fecha - INTERVAL '7 days' AND m.fecha) as avg_atencion, " +
        "  (SELECT AVG(t.rpe) FROM trainings t WHERE t.fecha BETWEEN m.fecha - INTERVAL '7 days' AND m.fecha) as avg_rpe, " +
        "  (SELECT COUNT(*) FROM trainings t WHERE t.fecha BETWEEN m.fecha - INTERVAL '7 days' AND m.fecha) as num_sesiones " +
        "FROM matches m WHERE m.status='PLAYED' AND m.nota > 0 ORDER BY m.fecha DESC LIMIT 30")
      var roiRows = List[(Double,Double,Double,Double,Int)]()
      while (rsROI.next()) {
        val nota = rsROI.getDouble("nota")
        val cal  = Option(rsROI.getObject("avg_calidad")).map(_.toString.toDouble).getOrElse(0.0)
        val ate  = Option(rsROI.getObject("avg_atencion")).map(_.toString.toDouble).getOrElse(0.0)
        val rpe  = Option(rsROI.getObject("avg_rpe")).map(_.toString.toDouble).getOrElse(0.0)
        val ses  = rsROI.getInt("num_sesiones")
        if (ses > 0) roiRows = roiRows :+ (nota, cal, ate, rpe, ses)
      }
      val roiCorrCalidad: Double  = calcCorrelation(roiRows.map(r => (r._2, r._1)))
      val roiCorrAtencion: Double = calcCorrelation(roiRows.map(r => (r._3, r._1)))
      val roiCorrCarga: Double    = calcCorrelation(roiRows.map(r => (r._4, r._1)))
      val roiPartidosConDatos: Int = roiRows.size
      val roiSesionesMedia: Double = if (roiRows.nonEmpty) roiRows.map(_._5.toDouble).sum / roiRows.size else 0.0

      // ── 6. Analisis IA Moneyball ─────────────────────────────────────────────
      val analisisIA: String = {
        val prompt = s"""Eres un analista de datos de porteros al nivel de los mejores clubes de Europa. Analiza estas metricas avanzadas:
xT_GK (distribucion): ${f"$xtScore%.2f"} pts/partido | Correlacion con nota: ${f"$xtCorr%.2f"}
xPoints (clutch): ${f"$xpMedia%.2f"} pts/partido | Clutch Rating: $clutchRating/100
SPV (sweeper): ${f"$spvMedia%.1f"} pts/partido | Paradas 1v1: $pct1v1% | Aereas: $pctAer%
Bypass Rate: ${f"$bpMedia%.1f"} lineas/partido | Eficiencia: ${f"${bpEfic*100}%.0f"}%
ROI Entreno: correlacion calidad-nota: ${f"$roiCorrCalidad%.2f"} | atencion-nota: ${f"$roiCorrAtencion%.2f"} | carga-nota: ${f"$roiCorrCarga%.2f"}
Responde en 3 partes exactas, en espanol, conciso y directo:
PATRON: [patron principal detectado en 1-2 frases]
VENTAJA: [metrica donde destaca mas y por que es relevante]
CONSEJO: [recomendacion de mejora basada en los datos]"""
        AIProvider.ask(prompt, None, bypassCache = false)
      }

      // ── 7. Etiquetas temporales para graficos ────────────────────────────────
      val rsLabels = conn.createStatement().executeQuery(
        "SELECT TO_CHAR(fecha,'MM-DD') as f FROM matches WHERE status='PLAYED' ORDER BY fecha DESC LIMIT 20")
      var labels = List[String]()
      while (rsLabels.next()) labels = labels :+ rsLabels.getString("f")
      val labelsRev: List[String] = labels.reverse

      Map(
        "xtScore"          -> xtScore,
        "xtCorr"           -> xtCorr,
        "xtSerie"          -> xtSerie,
        "xpMedia"          -> xpMedia,
        "xpTotal"          -> xpTotal,
        "xpSerie"          -> xpSerie,
        "clutchRating"     -> clutchRating,
        "spvScore"         -> spvScore,
        "spvMedia"         -> spvMedia,
        "spvSerie"         -> spvSerie,
        "spv1v1Pct"        -> pct1v1,
        "spvAerPct"        -> pctAer,
        "spvNorPct"        -> pctNor,
        "bpMedia"          -> bpMedia,
        "bpEfic"           -> bpEfic,
        "bpSerie"          -> bpSerie,
        "bpConDatos"       -> bpConDatos,
        "roiCorrCalidad"   -> roiCorrCalidad,
        "roiCorrAtencion"  -> roiCorrAtencion,
        "roiCorrCarga"     -> roiCorrCarga,
        "roiPartidos"      -> roiPartidosConDatos,
        "roiSesiones"      -> roiSesionesMedia,
        "analisisIA"       -> analisisIA,
        "labels"           -> labelsRev
      )
    } finally { conn.close() }
  }

  // Pearson correlation helper
  private def calcCorrelation(pairs: List[(Double, Double)]): Double = {
    if (pairs.size < 3) return 0.0
    val n = pairs.size.toDouble
    val meanX = pairs.map(_._1).sum / n
    val meanY = pairs.map(_._2).sum / n
    val num   = pairs.map(p => (p._1 - meanX) * (p._2 - meanY)).sum
    val denX  = math.sqrt(pairs.map(p => math.pow(p._1 - meanX, 2)).sum)
    val denY  = math.sqrt(pairs.map(p => math.pow(p._2 - meanY, 2)).sum)
    if (denX * denY == 0) 0.0 else num / (denX * denY)
  }

  def getDigitalTwinData(hPadre: Double, hMadre: Double): Map[String, Any] = {
    val conn = getConnection()
    try {
      // 1. Fecha de nacimiento y edad actual
      val rsP = conn.createStatement().executeQuery(
        "SELECT fecha_nacimiento FROM seasons ORDER BY id DESC LIMIT 1")
      val fechaNac = if (rsP.next())
        Option(rsP.getDate("fecha_nacimiento")).map(_.toString).getOrElse("2015-06-19")
      else "2015-06-19"
      val hoy = java.time.LocalDate.now()
      val nac = java.time.LocalDate.parse(fechaNac)
      val edadAnios  = java.time.Period.between(nac, hoy).getYears
      val edadMeses  = java.time.Period.between(nac, hoy).getYears * 12 +
                       java.time.Period.between(nac, hoy).getMonths

      // 2. Historial de crecimiento completo
      val rsG = conn.createStatement().executeQuery(
        "SELECT TO_CHAR(fecha,'YYYY-MM') as mes, altura, peso, velocidad_crecimiento FROM physical_growth ORDER BY fecha ASC")
      var growthRows = List[(String, Double, Double, Double)]()
      while (rsG.next()) growthRows = growthRows :+ (
        rsG.getString("mes"),
        rsG.getDouble("altura"),
        rsG.getDouble("peso"),
        rsG.getDouble("velocidad_crecimiento"))

      val alturaActual: Double = if (growthRows.nonEmpty) growthRows.last._2 else 120.0
      val pesoActual: Double   = if (growthRows.nonEmpty) growthRows.last._3 else 30.0

      // 3. Velocidad de crecimiento maxima = PHV detector (fallback por defecto)
      val velocidades = growthRows.map(_._4).filter(_ > 0)
      val phvVelocidad: Double = if (velocidades.nonEmpty) velocidades.max else 0.0
      val phvDetectado: Boolean = phvVelocidad >= 6.0  // >6cm/anio = pleno pico
      val fasePhvVelocidad: String = if (phvDetectado) "PICO ACTIVO" else if (edadAnios < 12) "PRE-PICO" else "POST-PICO"

      // 3b. PHV con formula de Mirwald (mas precisa) si hay talla sentado + longitud pierna registradas
      val rsAntro = conn.createStatement().executeQuery(
        "SELECT altura, peso, talla_sentado_cm, longitud_pierna_cm FROM physical_growth WHERE talla_sentado_cm IS NOT NULL AND longitud_pierna_cm IS NOT NULL ORDER BY fecha DESC LIMIT 1"
      )
      val (fasePhv: String, phvMaturityOffset: Option[Double]) = if (rsAntro.next()) {
        val altAntro = rsAntro.getDouble("altura")
        val pesoAntro = rsAntro.getDouble("peso")
        val tallaSentado = rsAntro.getDouble("talla_sentado_cm")
        val longitudPierna = rsAntro.getDouble("longitud_pierna_cm")
        val edadDecimal = edadMeses / 12.0
        val offset = -9.236 +
          0.0002708 * (longitudPierna * tallaSentado) -
          0.001663  * (edadDecimal * longitudPierna) +
          0.007216  * (edadDecimal * tallaSentado) +
          0.02292   * (if (altAntro > 0) pesoAntro / altAntro * 100 else 0.0)
        val fase = if (offset < -1.0) "PRE-PICO" else if (offset <= 1.0) "PICO ACTIVO" else "POST-PICO"
        (fase, Some(offset))
      } else (fasePhvVelocidad, None)

      // 4. Proyeccion de altura adulta
      // Base cientifica: Metodo Tanner midparent (correlacion genetica ~0.75)
      // chico = (hPadre + hMadre + 13) / 2  => rango +/- 8.5 cm (1 SD)
      val midParent: Double = if (hPadre > 0 && hMadre > 0) (hPadre + hMadre + 13.0) / 2.0 else 183.0

      // Tabla OMS: % de altura adulta alcanzado por edad (chicos)
      val pctPorEdad = Map(5->72.0, 6->75.0, 7->77.0, 8->80.0, 9->82.0, 10->84.0,
                           11->86.5, 12->89.0, 13->93.0, 14->97.0, 15->99.0, 16->100.0)

      // Altura actual inconsistente con la edad registrada
      val alturaConsistente: Boolean = alturaActual > 80 && !(edadAnios >= 12 && alturaActual < 130)

      // Factor extrapolado desde altura actual
      val factorAlturaActual: Double =
        if (alturaConsistente && edadAnios >= 5) {
          val pct = pctPorEdad.getOrElse(edadAnios, if(edadAnios > 16) 100.0 else 75.0)
          alturaActual / (pct / 100.0)
        } else midParent

      // Ponderacion dinamica por edad:
      // A menos edad, la extrapolacion desde la altura actual es menos fiable.
      // < 8 anos: 10% altura / 90% genetica
      // 8-11 anos: 25% altura / 75% genetica
      // 12-14 anos: 40% altura / 60% genetica
      // >= 15 anos: 60% altura / 40% genetica (ya casi adulto, los datos mandan)
      val pesoAlturaActual: Double =
        if (!alturaConsistente) 0.0
        else if (edadAnios < 8)  0.10
        else if (edadAnios < 12) 0.25
        else if (edadAnios < 15) 0.40
        else                     0.60

      val alturaProyRaw: Double = factorAlturaActual * pesoAlturaActual + midParent * (1.0 - pesoAlturaActual)

      // Suelo/techo: la proyeccion no puede alejarse mas de 1 SD del midparent (8.5 cm, Tanner)
      val alturaProyectada: Double = math.max(midParent - 8.5, math.min(midParent + 8.5, alturaProyRaw))
      val alturaMin: Double = alturaProyectada - 8.5
      val alturaMax: Double = alturaProyectada + 8.5
      val advertenciaFecha: Boolean = false  // fecha real confirmada

      // 5. Metricas de portero proyectadas
      val envergaduraActual: Double  = alturaActual  * 1.065
      val envergaduraAdulta: Double  = alturaProyectada * 1.065
      val alcanceActual: Double      = alturaActual  * 1.33
      val alcanceAdulto: Double      = alturaProyectada * 1.33
      // Cobertura porteria 7.32m x 2.44m = 17.86m2
      // Portero cubre aprox (envergadura * 2.5) m2 efectivos
      val coberturaActual: Double    = math.min(100.0, (envergaduraActual * 2.5 / 17.86) * 100)
      val coberturaAdulta: Double    = math.min(100.0, (envergaduraAdulta * 2.5 / 17.86) * 100)

      // 6. Comparativa con percentiles de porteros profesionales
      val pctAltura: Int = if (alturaProyectada >= 190) 90 else if (alturaProyectada >= 185) 75
                           else if (alturaProyectada >= 180) 50 else if (alturaProyectada >= 175) 25 else 10

      // 7. Evolucion de nota media por temporada para proyeccion de rendimiento
      val rsN = conn.createStatement().executeQuery(
        "SELECT id, nombre_club, categoria, media FROM seasons ORDER BY id ASC")
      var notaTemps = List[(String, Double)]()
      while (rsN.next()) {
        val label: String = Option(rsN.getString("nombre_club")).filter(_.nonEmpty).getOrElse("T" + rsN.getInt("id"))
        notaTemps = notaTemps :+ (label, rsN.getDouble("media"))
      }
      val tendenciaNota: Double = if (notaTemps.size >= 2) {
        val mejora = notaTemps.last._2 - notaTemps.head._2
        val aniosTranscurridos = notaTemps.size.toDouble
        mejora / aniosTranscurridos
      } else 0.5
      val notaActual: Double   = if (notaTemps.nonEmpty) notaTemps.last._2 else 60.0
      val aniosHasta18: Int    = math.max(0, 18 - edadAnios)
      val notaProyectada: Double = math.min(95.0, notaActual + tendenciaNota * aniosHasta18)

      // 8. Curva de crecimiento proyectada (puntos para el grafico)
      // Genera puntos desde edad actual hasta 18 anios
      val curvaProyeccion: List[(Int, Double)] = {
        val pctPorEdad2 = Map(7->77.0, 8->80.0, 9->82.0, 10->84.0, 11->86.5, 12->89.0,
                              13->93.0, 14->97.0, 15->99.0, 16->100.0, 17->100.0, 18->100.0)
        (edadAnios to 18).toList.map { edad =>
          val pct = pctPorEdad2.getOrElse(edad, 100.0)
          edad -> (alturaProyectada * pct / 100.0)
        }
      }

      // 9. Analisis IA del Digital Twin
      val analisisIA: String = {
        val prompt = s"""Eres un ojeador de elite y analista de rendimiento. Analiza el perfil proyectado de Hector, portero de $edadAnios anos:
Altura actual: ${alturaActual.toInt} cm | Proyeccion adulta: ${alturaProyectada.toInt} cm (rango ${alturaMin.toInt}-${alturaMax.toInt} cm)
Envergadura proyectada adulta: ${envergaduraAdulta.toInt} cm | Alcance de parada: ${alcanceAdulto.toInt} cm
Fase PHV: $fasePhv | Velocidad crecimiento maxima detectada: ${phvVelocidad.toInt} cm/anio
Nota media actual: ${notaActual.toInt} | Proyeccion nota a los 18 anos: ${notaProyectada.toInt}
Trayectoria: ${notaTemps.map(t => t._1+":"+t._2.toInt).mkString(", ")}
Responde en 4 partes exactas, en espanol, conciso y directo:
BIOTIPO: [descripcion del perfil fisico proyectado en 1-2 frases]
VENTAJA: [principal ventaja competitiva de su biotipo para ser portero en 1 frase]
RIESGO: [1 riesgo o area de mejora fisica concreta]
PROYECCION: [nivel al que podria llegar segun datos actuales, en 1 frase motivadora]"""
        AIProvider.ask(prompt, None, bypassCache = false)
      }

      Map(
        "edadAnios"         -> edadAnios,
        "alturaActual"      -> alturaActual,
        "pesoActual"        -> pesoActual,
        "alturaProyectada"  -> alturaProyectada,
        "alturaMin"         -> alturaMin,
        "alturaMax"         -> alturaMax,
        "envergaduraActual" -> envergaduraActual,
        "envergaduraAdulta" -> envergaduraAdulta,
        "alcanceActual"     -> alcanceActual,
        "alcanceAdulto"     -> alcanceAdulto,
        "coberturaActual"   -> coberturaActual,
        "coberturaAdulta"   -> coberturaAdulta,
        "pctAltura"         -> pctAltura,
        "phvVelocidad"      -> phvVelocidad,
        "fasePhv"           -> fasePhv,
        "phvMaturityOffset" -> phvMaturityOffset,
        "notaActual"        -> notaActual,
        "notaProyectada"    -> notaProyectada,
        "tendenciaNota"     -> tendenciaNota,
        "growthRows"        -> growthRows,
        "curvaProyeccion"   -> curvaProyeccion,
        "notaTemps"         -> notaTemps,
        "analisisIA"        -> analisisIA,
        "hPadre"            -> hPadre,
        "hMadre"            -> hMadre,
        "midParent"         -> midParent,
        "advertenciaFecha"  -> advertenciaFecha
      )
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE A — GOAL COVERAGE MAPPING (geometria pura, sin Gemini)
  // ─────────────────────────────────────────────────────────────────────────────
  // tallaAdultaCmOverride: si se pasa (>0) se usa esa proyeccion adulta (p.ej. la ya
  // calculada en /digital-twin con las alturas de los padres reales); si no, se calcula
  // con la proyeccion por defecto (180/168) del propio Digital Twin.
  def calcularGoalCoverage(tallaAdultaCmOverride: Double = 0.0): Map[String, Any] = {
    // Dimensiones porteria Futbol 7 Prebenjamin (reglamento RFFM)
    val porteriaAncho = 5.0  // metros
    val porteriaAlto  = 2.0  // metros
    val areaPorteria  = porteriaAncho * porteriaAlto

    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT altura FROM physical_growth WHERE altura IS NOT NULL ORDER BY fecha DESC LIMIT 1")
      val tallaCm = if (rs.next()) rs.getDouble("altura") else 118.0
      val tallaM = tallaCm / 100.0

      // Ratios anatomicos validados
      val envergadura = tallaM * 1.06       // envergadura ~= altura x 1.06
      val alcanceVertical = tallaM * 1.27   // alcance vertical de pie
      val longitudBrazo = envergadura / 2.0

      // Cobertura teorica en posicion central
      val anchoCubierto = math.min(envergadura, porteriaAncho)
      val altoCubierto  = math.min(alcanceVertical, porteriaAlto)
      val areaCubiertaBase = anchoCubierto * altoCubierto
      val pctCoberturaBase = (areaCubiertaBase / areaPorteria) * 100

      // Cobertura con estirada lateral (alcance de un brazo a cada lado)
      val anchoConEstirada = math.min(envergadura + longitudBrazo, porteriaAncho)
      val areaCubiertaEstirada = anchoConEstirada * altoCubierto
      val pctCoberturaEstirada = (areaCubiertaEstirada / areaPorteria) * 100

      // Proyeccion adulta (usa la proyeccion ya calculada por el Digital Twin)
      val tallaAdultaCm = if (tallaAdultaCmOverride > 0) tallaAdultaCmOverride
        else getDigitalTwinData(180.0, 168.0).get("alturaProyectada").map(_.asInstanceOf[Double]).getOrElse(185.0)
      val tallaAdultaM = tallaAdultaCm / 100.0
      val envergaduraAdulta = tallaAdultaM * 1.06
      val alcanceAdulto = tallaAdultaM * 1.27
      val anchoAdultoEstirada = math.min(envergaduraAdulta + (envergaduraAdulta / 2.0), porteriaAncho)
      val pctCoberturaAdulto = (math.min(anchoAdultoEstirada, porteriaAncho) *
        math.min(alcanceAdulto, porteriaAlto) / areaPorteria) * 100

      Map(
        "tallaCm"              -> tallaCm,
        "envergaduraCm"        -> (envergadura * 100),
        "alcanceVerticalCm"    -> (alcanceVertical * 100),
        "pctCoberturaBase"     -> pctCoberturaBase,
        "pctCoberturaEstirada" -> pctCoberturaEstirada,
        "tallaAdultaCm"        -> tallaAdultaCm,
        "pctCoberturaAdulto"   -> pctCoberturaAdulto,
        "porteriaAncho"        -> porteriaAncho,
        "porteriaAlto"         -> porteriaAlto
      )
    } finally { conn.close() }
  }

  // ── EFECTO MARIPOSA ──────────────────────────────────────────────────────
  // BLOQUE B3: seasonId=0 = historico completo (comportamiento anterior, sin cambios)
  def getEfectoMariposa(seasonId: Int = 0): Map[String, Any] = {
    val conn = getConnection()
    try {
      val sf = seasonFilter(seasonId)
      val rs = conn.createStatement().executeQuery(s"""
        SELECT
          COUNT(*) as pj,
          SUM(CASE WHEN goles_contra = 0 THEN 1 ELSE 0 END) as clean_sheets,
          SUM(CASE WHEN goles_contra = 0 AND goles_favor > goles_contra THEN 1 ELSE 0 END) as cs_wins,
          SUM(CASE WHEN goles_favor > goles_contra THEN 1 ELSE 0 END) as ganados,
          SUM(CASE WHEN goles_favor = goles_contra THEN 1 ELSE 0 END) as empatados,
          SUM(CASE WHEN goles_favor < goles_contra THEN 1 ELSE 0 END) as perdidos,
          AVG(nota) as nota_media
        FROM matches WHERE status = 'PLAYED' $sf
      """)
      if (!rs.next()) return Map("ok" -> false)
      val pj         = rs.getInt("pj")
      val cs         = rs.getInt("clean_sheets")
      val csWins     = rs.getInt("cs_wins")
      val ganados    = rs.getInt("ganados")
      val empatados  = rs.getInt("empatados")
      val perdidos   = rs.getInt("perdidos")
      val notaMedia  = rs.getDouble("nota_media")

      // Win rate con y sin clean sheet
      val csWinRate  = if (cs > 0) (csWins.toDouble / cs * 100).toInt else 0
      val nonCsWins  = ganados - csWins
      val nonCs      = pj - cs
      val nonCsWinRate = if (nonCs > 0) (nonCsWins.toDouble / nonCs * 100).toInt else 0

      // Clutch points: partidos ganados donde margen = 1 gol con nota >= 7.5
      val rsClutch = conn.createStatement().executeQuery(s"""
        SELECT COUNT(*) as clutch,
               SUM(goles_favor - goles_contra) as margen_total
        FROM matches
        WHERE status = 'PLAYED' $sf
          AND goles_favor > goles_contra
          AND (goles_favor - goles_contra) = 1
          AND nota >= 7.5
      """)
      val (clutch, margenTotal) = if (rsClutch.next())
        (rsClutch.getInt("clutch"), rsClutch.getInt("margen_total")) else (0, 0)

      // Influence data: nota por resultado para gráfico
      val rsInfluence = conn.createStatement().executeQuery(s"""
        SELECT
          CASE WHEN goles_favor > goles_contra THEN 'G'
               WHEN goles_favor = goles_contra THEN 'E'
               ELSE 'P' END as res,
          ROUND(nota::numeric, 1) as nota,
          COUNT(*) as cnt
        FROM matches WHERE status = 'PLAYED' $sf
        GROUP BY res, ROUND(nota::numeric, 1)
        ORDER BY nota
      """)
      var influenceData = List[Map[String, Any]]()
      while (rsInfluence.next()) {
        influenceData = influenceData :+ Map(
          "res"  -> rsInfluence.getString("res"),
          "nota" -> rsInfluence.getDouble("nota"),
          "cnt"  -> rsInfluence.getInt("cnt")
        )
      }

      Map(
        "ok"           -> true,
        "pj"           -> pj,
        "cleanSheets"  -> cs,
        "csWinRate"    -> csWinRate,
        "nonCsWinRate" -> nonCsWinRate,
        "ganados"      -> ganados,
        "empatados"    -> empatados,
        "perdidos"     -> perdidos,
        "notaMedia"    -> notaMedia,
        "clutchPoints" -> clutch,
        "influenceData"-> influenceData
      )
    } finally { conn.close() }
  }

  // ── GEAR ROI ──────────────────────────────────────────────────────────────
  def getGearROI(): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      // Get all gear with their usage and price
      val rs = conn.createStatement().executeQuery("""
        SELECT g.id, g.nombre, g.tipo, g.usos_actuales, g.vida_util_estimada,
               g.activo, COALESCE(g.precio_compra, 0) as precio,
               AVG(m.nota) as nota_media,
               SUM(CASE WHEN m.clima ILIKE '%lluv%' OR m.clima ILIKE '%rain%' THEN 1 ELSE 0 END) as partidos_lluvia
        FROM gear g
        LEFT JOIN matches m ON m.status = 'PLAYED'
          AND m.fecha >= (SELECT MIN(fecha) FROM matches WHERE status='PLAYED')
        GROUP BY g.id, g.nombre, g.tipo, g.usos_actuales, g.vida_util_estimada, g.activo, g.precio_compra
        ORDER BY g.activo DESC, g.usos_actuales DESC
      """)
      var list = List[Map[String, Any]]()
      while (rs.next()) {
        val usos   = rs.getInt("usos_actuales")
        val vida   = rs.getInt("vida_util_estimada")
        val precio = rs.getDouble("precio")
        val nota   = rs.getDouble("nota_media")
        val lluvia = rs.getInt("partidos_lluvia")
        val desgaste = if (vida > 0) (usos.toDouble / vida * 100).toInt else 0
        val costePorPartido = if (usos > 0 && precio > 0) precio / usos else 0.0

        // Grip alert: >60% desgaste base + lluvia multiplier
        val gripLoss = math.min(100, desgaste + (lluvia * 3))
        val gripAlert = gripLoss >= 70

        list = list :+ Map(
          "id"              -> rs.getInt("id"),
          "nombre"          -> rs.getString("nombre"),
          "tipo"            -> rs.getString("tipo"),
          "usos"            -> usos,
          "vida"            -> vida,
          "activo"          -> rs.getBoolean("activo"),
          "precio"          -> precio,
          "desgaste"        -> desgaste,
          "gripLoss"        -> gripLoss,
          "gripAlert"       -> gripAlert,
          "costePorPartido" -> costePorPartido,
          "notaMedia"       -> nota,
          "partidosLluvia"  -> lluvia
        )
      }
      list
    } finally { conn.close() }
  }

  def updateGearPrecio(gearId: Int, precio: Double): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("UPDATE gear SET precio_compra = ? WHERE id = ?")
      ps.setDouble(1, precio); ps.setInt(2, gearId)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def getTacticalStats(): Map[String, Int] = { var stats = scala.collection.mutable.Map("g_tot"->0, "g_alt"->0, "g_med"->0, "g_ras"->0, "g_izq"->0, "g_cen"->0, "g_der"->0, "p_tot"->0, "p_alt"->0, "p_med"->0, "p_ras"->0, "p_izq"->0, "p_cen"->0, "p_der"->0); val conn = getConnection(); try { val rs = conn.createStatement().executeQuery("SELECT zona_goles, zona_paradas FROM matches WHERE status='PLAYED' ORDER BY id DESC LIMIT 20"); while(rs.next()) { val zG = Option(rs.getString("zona_goles")).getOrElse(""); val zP = Option(rs.getString("zona_paradas")).getOrElse(""); zG.split(",").filter(_.nonEmpty).foreach { z => stats("g_tot")+=1; if(z.contains("T")) stats("g_alt")+=1 else if(z.contains("M")) stats("g_med")+=1 else stats("g_ras")+=1; if(z.contains("L")) stats("g_izq")+=1 else if(z.contains("C")) stats("g_cen")+=1 else stats("g_der")+=1 }; zP.split(",").filter(_.nonEmpty).foreach { z => stats("p_tot")+=1; if(z.contains("T")) stats("p_alt")+=1 else if(z.contains("M")) stats("p_med")+=1 else stats("p_ras")+=1; if(z.contains("L")) stats("p_izq")+=1 else if(z.contains("C")) stats("p_cen")+=1 else stats("p_der")+=1 } } } finally { conn.close() }; stats.toMap }
  def updateStats(s: PlayerCardData): Unit = { val conn=getConnection(); try { val st=conn.prepareStatement("UPDATE seasons SET media=?, stat_div=?, stat_han=?, stat_kic=?, stat_ref=?, stat_spd=?, stat_pos=? WHERE id=(SELECT MAX(id) FROM seasons)"); st.setDouble(1,s.media); st.setDouble(2,s.divRaw); st.setDouble(3,s.hanRaw); st.setDouble(4,s.kicRaw); st.setDouble(5,s.refRaw); st.setDouble(6,s.spdRaw); st.setDouble(7,s.posRaw); st.executeUpdate() } finally { conn.close() } }
  def getBackupCSV(): String = { val sb=new StringBuilder(); sb.append("RIVAL,GF,GC,MIN,NOTA,PARADAS,CLIMA,ESTADIO,NOTAS,REACCION,FECHA\n"); val conn=getConnection(); try{ val rs=conn.createStatement().executeQuery("SELECT * FROM matches WHERE status='PLAYED' ORDER BY fecha ASC"); while(rs.next()){ sb.append(s"${rs.getString("rival")},${rs.getInt("goles_favor")},${rs.getInt("goles_contra")},${rs.getInt("minutos")},${rs.getDouble("nota")},${rs.getInt("paradas")},${Option(rs.getString("clima")).getOrElse("Sol")},${Option(rs.getString("estadio")).getOrElse("-")},${Option(rs.getString("notas_partido")).getOrElse("")},${Option(rs.getString("reaccion_goles")).getOrElse("")},${rs.getDate("fecha")}\n") } } finally {conn.close()}; sb.toString() }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE H — EXPORTACION COMPLETA EN CSV (una tabla por fichero, empaquetados en ZIP)
  // ─────────────────────────────────────────────────────────────────────────────
  // Genera un CSV generico a partir de SELECT * usando el metadata del ResultSet,
  // para no tener que mantener manualmente la lista de columnas de cada tabla.
  private def tableToCSV(conn: Connection, query: String): String = {
    val rs = conn.createStatement().executeQuery(query)
    val meta = rs.getMetaData
    val n = meta.getColumnCount
    val sb = new StringBuilder()
    sb.append((1 to n).map(meta.getColumnName).mkString(",")).append("\n")
    def csvSafe(v: String): String = {
      val trunc = if (v.length > 2000) v.take(2000) + "..." else v
      trunc.replace("\r", " ").replace("\n", " ").replace(",", ";")
    }
    while (rs.next()) {
      val row = (1 to n).map(i => csvSafe(Option(rs.getString(i)).getOrElse("")))
      sb.append(row.mkString(",")).append("\n")
    }
    sb.toString()
  }

  def getFullExportCSV(): Map[String, String] = {
    val conn = getConnection()
    try {
      Map(
        "partidos.csv"        -> tableToCSV(conn, "SELECT * FROM matches ORDER BY id ASC"),
        "entrenamientos.csv"  -> tableToCSV(conn, "SELECT * FROM trainings ORDER BY id ASC"),
        "wellness.csv"        -> tableToCSV(conn, "SELECT * FROM wellness ORDER BY id ASC"),
        "crecimiento.csv"     -> tableToCSV(conn, "SELECT * FROM physical_growth ORDER BY id ASC"),
        "skills.csv"          -> tableToCSV(conn, "SELECT * FROM goalkeeper_skills ORDER BY id ASC"),
        "test_fisicos.csv"    -> tableToCSV(conn, "SELECT * FROM physical_tests ORDER BY id ASC"),
        "cognitivo.csv"       -> tableToCSV(conn, "SELECT * FROM cognitivo_tests ORDER BY id ASC"),
        "psicologico.csv"     -> tableToCSV(conn, "SELECT * FROM psych_records ORDER BY id ASC"),
        "lesiones.csv"        -> tableToCSV(conn, "SELECT * FROM injuries ORDER BY id ASC"),
        "contactos.csv"       -> tableToCSV(conn, "SELECT * FROM contacts ORDER BY id ASC"),
        "oportunidades.csv"   -> tableToCSV(conn, "SELECT * FROM opportunities ORDER BY id ASC"),
        "voz_portero.csv"     -> tableToCSV(conn, "SELECT * FROM voz_portero ORDER BY fecha ASC, id ASC"),
        "arquetipo_history.csv" -> tableToCSV(conn, "SELECT * FROM arquetipo_history ORDER BY id ASC"),
        "hitos_conseguidos.csv" -> tableToCSV(conn, "SELECT * FROM hitos_conseguidos ORDER BY fecha ASC, id ASC")
      )
    } finally { conn.close() }
  }
  def updateObjective(id: Int, meta: Int): Unit = { val conn=getConnection(); try{ val ps=conn.prepareStatement("UPDATE objectives SET meta=? WHERE id=?"); ps.setInt(1,meta); ps.setInt(2,id); ps.executeUpdate() } finally {conn.close()} }
  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE 2 — GESTION COMPLETA DE TEMPORADAS
  // ─────────────────────────────────────────────────────────────────────────────
  def cerrarTemporadaActual(): Either[String, String] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("""
        SELECT s.id, s.nombre, s.categoria,
          COUNT(m.id) as pj,
          COALESCE(AVG(m.nota), 0) as media,
          SUM(CASE WHEN m.goles_contra = 0 THEN 1 ELSE 0 END) as cs
        FROM seasons s
        LEFT JOIN matches m ON m.season_id = s.id AND m.status = 'PLAYED'
        WHERE s.id = (SELECT MAX(id) FROM seasons)
        GROUP BY s.id, s.nombre, s.categoria""")
      if (!rs.next()) return Left("No hay temporada activa.")
      val (sid, nombre, cat, pj, media, cs) = (
        rs.getInt("id"), Option(rs.getString("nombre")).getOrElse(rs.getString("categoria")), rs.getString("categoria"),
        rs.getInt("pj"), rs.getDouble("media"), rs.getInt("cs"))
      if (pj < 1) return Left(s"La temporada '$nombre' no tiene partidos jugados. No se puede cerrar.")
      val ps = conn.prepareStatement(
        "UPDATE seasons SET fecha_fin=CURRENT_DATE, media_final=?, porterias_cero_total=? WHERE id=?")
      ps.setDouble(1, media); ps.setInt(2, cs); ps.setInt(3, sid)
      ps.executeUpdate()
      conn.createStatement().executeUpdate(
        s"UPDATE matches SET status='CANCELLED' WHERE season_id=$sid AND status='SCHEDULED'")
      Right(s"Temporada '$nombre — $cat' cerrada — $pj partidos · media ${"%.1f".format(media)} · $cs porterías a cero.")
    } finally { conn.close() }
  }

  def startNewSeason(categoria: String, nombreClub: String = "", fechaInicio: String = ""): Either[String, String] = {
    val conn = getConnection()
    try {
      val rsCheck = conn.createStatement().executeQuery(
        "SELECT id, COALESCE(nombre, categoria) as nombre, fecha_fin FROM seasons ORDER BY id DESC LIMIT 1")
      if (rsCheck.next() && Option(rsCheck.getDate("fecha_fin")).isEmpty)
        return Left(s"Cierra primero la temporada '${rsCheck.getString("nombre")}' antes de crear una nueva.")

      // FIX 4B: valida que fechaInicio tenga formato yyyy-MM-dd antes de usarla para calcular
      // el nombre de temporada. Un valor malformado (ej. llegado sin pasar por el input type=date
      // del navegador) puede producir años corruptos como "20206" en nombreTemporada.
      val fechaValida = fechaInicio.nonEmpty && fechaInicio.matches("""\d{4}-\d{2}-\d{2}""")
      val inicio = if (fechaValida) fechaInicio else java.time.LocalDate.now().toString
      val anioInicio = java.time.LocalDate.parse(inicio).getYear
      val nombreTemporada = s"$anioInicio-${(anioInicio + 1).toString.takeRight(2)}"

      val rsPrev = conn.createStatement().executeQuery(
        "SELECT * FROM seasons ORDER BY id DESC LIMIT 1")
      val hayPrev = rsPrev.next()

      // A1: la BD de produccion tiene columnas NOT NULL sin DEFAULT que no declara initDB (ej. "tipo").
      // Se descubren en information_schema y se heredan de la temporada anterior; "tipo" cae a 'PORTERO'.
      val columnasFijas = Set("id", "nombre", "categoria", "nombre_club", "foto_jugador_url", "club_escudo_url",
        "media", "stat_div", "stat_han", "stat_kic", "stat_ref", "stat_spd", "stat_pos",
        "fecha_inicio", "fecha_nacimiento", "judo_belt")
      val rsCols = conn.createStatement().executeQuery("""
        SELECT column_name FROM information_schema.columns
        WHERE table_schema = current_schema() AND table_name = 'seasons'
          AND is_nullable = 'NO' AND column_default IS NULL""")
      val columnasExtra = Iterator.continually(rsCols).takeWhile(_.next()).map(_.getString(1))
        .filterNot(columnasFijas.contains).toList
      val valoresExtra: List[(String, AnyRef)] = columnasExtra.map { col =>
        val heredado = if (hayPrev) Option(rsPrev.getObject(col)) else None
        col -> heredado.orElse(if (col == "tipo") Some("PORTERO") else None).orNull
      }
      valoresExtra.find(_._2 == null).foreach { case (col, _) =>
        return Left(s"La columna obligatoria '$col' de seasons no tiene valor que heredar de la temporada anterior.")
      }

      val (club, foto, escudo, div, han, kic, ref, spd, pos, cinturon, fechaNac) =
        if (hayPrev) (
          if (nombreClub.nonEmpty) nombreClub else Option(rsPrev.getString("nombre_club")).getOrElse(""),
          Option(rsPrev.getString("foto_jugador_url")).getOrElse(""),
          Option(rsPrev.getString("club_escudo_url")).getOrElse(""),
          rsPrev.getDouble("stat_div"), rsPrev.getDouble("stat_han"), rsPrev.getDouble("stat_kic"),
          rsPrev.getDouble("stat_ref"), rsPrev.getDouble("stat_spd"), rsPrev.getDouble("stat_pos"),
          Option(rsPrev.getString("judo_belt")).getOrElse("Blanco"),
          Option(rsPrev.getDate("fecha_nacimiento")).map(_.toString).getOrElse("2020-06-19")
        ) else (nombreClub, "", "", 62.0, 60.0, 55.0, 60.0, 62.0, 58.0, "Blanco", "2020-06-19")

      val colsExtraSql = valoresExtra.map(c => s", ${c._1}").mkString
      val paramsExtraSql = valoresExtra.map(_ => ",?").mkString
      val ps = conn.prepareStatement(s"""
        INSERT INTO seasons
          (nombre, categoria, nombre_club, foto_jugador_url, club_escudo_url,
           media, stat_div, stat_han, stat_kic, stat_ref, stat_spd, stat_pos,
           fecha_inicio, fecha_nacimiento, judo_belt$colsExtraSql)
        VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?::date,?::date,?$paramsExtraSql)""")
      ps.setString(1, nombreTemporada); ps.setString(2, fixEncoding(categoria))
      ps.setString(3, fixEncoding(club)); ps.setString(4, foto); ps.setString(5, escudo)
      ps.setDouble(6, (div+han+kic+ref+spd+pos)/6.0)
      ps.setDouble(7, div); ps.setDouble(8, han); ps.setDouble(9, kic)
      ps.setDouble(10, ref); ps.setDouble(11, spd); ps.setDouble(12, pos)
      ps.setString(13, inicio); ps.setString(14, fechaNac); ps.setString(15, cinturon)
      valoresExtra.zipWithIndex.foreach { case ((_, v), i) => ps.setObject(16 + i, v) }
      ps.executeUpdate()

      conn.createStatement().executeUpdate("DELETE FROM ai_cache")
      conn.createStatement().executeUpdate("DELETE FROM feature_cache")
      conn.createStatement().executeUpdate("DELETE FROM forma_diaria")
      conn.createStatement().executeUpdate("DELETE FROM micro_objetivos")
      conn.createStatement().executeUpdate(
        "UPDATE idp_temporadas SET estado='COMPLETADA' WHERE estado='ACTIVA'")

      Right(s"Nueva temporada '$nombreTemporada — $categoria' iniciada. Reset: caché IA, forma diaria, micro-objetivos. Conservado: historial completo.")
    } finally { conn.close() }
  }

  /** Genera en background el informe de valoracion final de una temporada cerrada y lo guarda en seasons.informe_fin_temporada. */
  def generarInformeFinTemporada(seasonId: Int): Unit = {
    new Thread(() => {
      val conn = getConnection()
      try {
        val rs = conn.prepareStatement("SELECT nombre, categoria, fecha_nacimiento FROM seasons WHERE id=?")
        rs.setInt(1, seasonId)
        val rsRes = rs.executeQuery()
        if (rsRes.next()) {
          val nombreTemp = Option(rsRes.getString("nombre")).getOrElse(rsRes.getString("categoria"))
          val cat = rsRes.getString("categoria")
          val edad = calcularEdadExacta(Option(rsRes.getDate("fecha_nacimiento")).map(_.toString).getOrElse("2020-06-19"))

          val rsStats = conn.prepareStatement("""
            SELECT COUNT(*) as pj,
              SUM(CASE WHEN goles_favor > goles_contra THEN 1 ELSE 0 END) as pg,
              SUM(CASE WHEN goles_favor = goles_contra THEN 1 ELSE 0 END) as pe,
              SUM(CASE WHEN goles_favor < goles_contra THEN 1 ELSE 0 END) as pp,
              COALESCE(SUM(goles_favor),0) as gf, COALESCE(SUM(goles_contra),0) as gc,
              COALESCE(AVG(nota),0) as media,
              SUM(CASE WHEN goles_contra=0 THEN 1 ELSE 0 END) as cs
            FROM matches WHERE season_id=? AND status='PLAYED'""")
          rsStats.setInt(1, seasonId)
          val st = rsStats.executeQuery()
          st.next()
          val (pj, pg, pe, pp, gf, gc, media, cs) = (st.getInt("pj"), st.getInt("pg"), st.getInt("pe"), st.getInt("pp"), st.getInt("gf"), st.getInt("gc"), st.getDouble("media"), st.getInt("cs"))

          val rsMejor = conn.prepareStatement("SELECT rival, fecha, nota FROM matches WHERE season_id=? AND status='PLAYED' ORDER BY nota DESC LIMIT 1")
          rsMejor.setInt(1, seasonId)
          val bm = rsMejor.executeQuery()
          val mejorPartido = if (bm.next()) s"${bm.getString("rival")} (${bm.getDate("fecha")}) nota ${bm.getDouble("nota")}" else "N/D"

          val rsPeor = conn.prepareStatement("SELECT rival, fecha, nota FROM matches WHERE season_id=? AND status='PLAYED' ORDER BY nota ASC LIMIT 1")
          rsPeor.setInt(1, seasonId)
          val pm = rsPeor.executeQuery()
          val peorPartido = if (pm.next()) s"${pm.getString("rival")} (${pm.getDate("fecha")}) nota ${pm.getDouble("nota")}" else "N/D"

          val rsMes = conn.prepareStatement("""
            SELECT TO_CHAR(fecha, 'YYYY-MM') as mes, AVG(nota) as media
            FROM matches WHERE season_id=? AND status='PLAYED' GROUP BY mes ORDER BY mes ASC""")
          rsMes.setInt(1, seasonId)
          val rsMesR = rsMes.executeQuery()
          val evolucionMensual = new StringBuilder()
          while (rsMesR.next()) evolucionMensual.append(s"${rsMesR.getString("mes")}: ${"%.1f".format(rsMesR.getDouble("media"))}; ")

          val statsStr = s"PJ=$pj PG=$pg PE=$pe PP=$pp GF=$gf GC=$gc Media=${"%.1f".format(media)} PorteriasCero=$cs. Mejor partido: $mejorPartido. Peor partido: $peorPartido. Evolucion mensual: ${evolucionMensual.toString}"

          val prompt = s"""Eres el director deportivo que hace la valoración final de temporada de Héctor, portero de $edad años. Temporada $nombreTemp — $cat. Datos: $statsStr. Genera en HTML limpio: <h2>VALORACIÓN FINAL — $nombreTemp</h2>, <h3>Rendimiento</h3> (párrafo evaluando la temporada en conjunto), <h3>Logro destacado</h3> (el mejor momento y por qué fue especial), <h3>Área de mejora prioritaria</h3> (lo más importante para la siguiente temporada), <h3>Carta al portero</h3> (párrafo emotivo en segunda persona dirigido a Héctor, como si fuera una carta de su entrenador que leerá cuando sea mayor). Lenguaje humano, no corporativo. Que sea memorable."""

          val informe = AIProvider.ask(prompt, None, bypassCache = true).replace("```html", "").replace("```", "").trim

          val up = conn.prepareStatement("UPDATE seasons SET informe_fin_temporada=? WHERE id=?")
          up.setString(1, informe); up.setInt(2, seasonId)
          up.executeUpdate()
        }
      } catch { case e: Exception => println(s"[!] generarInformeFinTemporada error: ${e.getMessage}") }
      finally { conn.close() }
    }).start()
  }

  def getInformeFinTemporada(seasonId: Int): Option[(String, String)] = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("SELECT COALESCE(nombre, categoria) as nombre, informe_fin_temporada FROM seasons WHERE id=?")
      ps.setInt(1, seasonId)
      val rs = ps.executeQuery()
      if (rs.next()) Some((rs.getString("nombre"), Option(rs.getString("informe_fin_temporada")).getOrElse("")))
      else None
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE B1 — ARQUITECTURA DE TEMPORADAS: HELPERS (Elite exclusivamente)
  // ─────────────────────────────────────────────────────────────────────────────
  def getTemporadaActivaId(): Int = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT id FROM seasons WHERE fecha_fin IS NULL ORDER BY id DESC LIMIT 1")
      if (rs.next()) rs.getInt("id") else 0
    } finally { conn.close() }
  }

  def getTodasTemporadas(): List[Map[String, Any]] = {
    var result = List[Map[String, Any]]()
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT id, COALESCE(nombre, categoria, 'Temporada') as nombre, categoria, fecha_inicio, fecha_fin FROM seasons ORDER BY id DESC")
      while (rs.next()) {
        val fechaFin = Option(rs.getDate("fecha_fin")).map(_.toString)
        result = result :+ Map(
          "id"          -> rs.getInt("id"),
          "nombre"      -> fixEncoding(Option(rs.getString("nombre")).getOrElse("")),
          "categoria"   -> Option(rs.getString("categoria")).getOrElse(""),
          "fechaInicio" -> Option(rs.getDate("fecha_inicio")).map(_.toString).getOrElse(""),
          "fechaFin"    -> fechaFin.getOrElse(""),
          "activa"      -> fechaFin.isEmpty
        )
      }
    } finally { conn.close() }
    result
  }

  // No privado: los controllers que construyen SQL inline (ej. /scanning-rate) tambien lo necesitan.
  // DECISION: seasonId=0 significa "sin filtro" (todo el historico), NO "temporada activa".
  // Esto es lo unico que hace cierta la premisa "el valor por defecto 0 garantiza que todos los
  // sitios existentes siguen funcionando sin cambios": getMatchesList(), getStrikerClusters(), etc.
  // se llaman desde muchos sitios fuera de las paginas del Bloque B4 (dashboard, admin, perfil
  // publico, scouting report, moneyball...) que esperan el historico completo de la carrera, no solo
  // la temporada activa. Las paginas del B4 resuelven "por defecto la temporada activa" ellas mismas
  // (val efectivo = if (temporadaId > 0) temporadaId else getTemporadaActivaId()) antes de llamar aqui.
  // Filtro de temporada centralizado — no escribir "season_id = ..." a mano en las consultas.
  //   seasonFilter(id, alias): opcional; id <= 0 significa "todas las temporadas".
  //   seasonFilterActual(alias): siempre la temporada actual (temporadaActualSQL).
  def seasonFilter(seasonId: Int, alias: String = ""): String =
    if (seasonId > 0) s"AND ${if (alias.isEmpty) "" else alias + "."}season_id = $seasonId" else ""

  /** Temporada actual: la abierta (fecha_fin NULL) mas reciente o, si no hay ninguna abierta, la ultima. */
  val temporadaActualSQL: String =
    "COALESCE((SELECT id FROM seasons WHERE fecha_fin IS NULL ORDER BY id DESC LIMIT 1), (SELECT MAX(id) FROM seasons))"

  def seasonFilterActual(alias: String = ""): String =
    s"AND ${if (alias.isEmpty) "" else alias + "."}season_id = $temporadaActualSQL"

  def getTemporadaActivaInfo(): Option[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("""
        SELECT s.id, COALESCE(s.nombre, s.categoria) as nombre, s.categoria, s.fecha_inicio, s.fecha_fin,
          (SELECT COUNT(*) FROM matches m WHERE m.season_id=s.id AND m.status='PLAYED') as pj,
          (SELECT COALESCE(AVG(nota),0) FROM matches m WHERE m.season_id=s.id AND m.status='PLAYED') as media
        FROM seasons s ORDER BY s.id DESC LIMIT 1""")
      if (rs.next()) Some(Map(
        "id" -> rs.getInt("id"), "nombre" -> rs.getString("nombre"), "categoria" -> rs.getString("categoria"),
        "fechaInicio" -> Option(rs.getDate("fecha_inicio")).map(_.toString).getOrElse(""),
        "fechaFin" -> Option(rs.getDate("fecha_fin")).map(_.toString).getOrElse(""),
        "pj" -> rs.getInt("pj"), "media" -> rs.getDouble("media")
      )) else None
    } finally { conn.close() }
  }

  def getTemporadasCerradas(): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("""
        SELECT id, COALESCE(nombre, categoria) as nombre, fecha_inicio, fecha_fin, media_final, porterias_cero_total,
          (SELECT COUNT(*) FROM matches m WHERE m.season_id=seasons.id AND m.status='PLAYED') as pj,
          (informe_fin_temporada IS NOT NULL AND informe_fin_temporada != '') as tiene_informe
        FROM seasons WHERE fecha_fin IS NOT NULL ORDER BY id DESC""")
      var l = List[Map[String, Any]]()
      while (rs.next()) l = l :+ Map(
        "id" -> rs.getInt("id"), "nombre" -> rs.getString("nombre"),
        "fechaInicio" -> Option(rs.getDate("fecha_inicio")).map(_.toString).getOrElse(""),
        "fechaFin" -> Option(rs.getDate("fecha_fin")).map(_.toString).getOrElse(""),
        "pj" -> rs.getInt("pj"), "mediaFinal" -> rs.getDouble("media_final"),
        "porteriasCero" -> rs.getInt("porterias_cero_total"), "tieneInforme" -> rs.getBoolean("tiene_informe")
      )
      l
    } finally { conn.close() }
  }
  def getCareerSummary(): List[SeasonSummary] = { var l=List[SeasonSummary](); val conn=getConnection(); try{ val rs=conn.createStatement().executeQuery("SELECT s.id, s.categoria, s.club_escudo_url, s.foto_jugador_url, s.media, (SELECT COUNT(*) FROM matches m WHERE m.season_id=s.id AND m.status='PLAYED') as pj, (SELECT SUM(goles_contra) FROM matches m WHERE m.season_id=s.id AND m.status='PLAYED') as gc FROM seasons s ORDER BY s.id DESC"); while(rs.next()){ l=l:+SeasonSummary(rs.getInt("id"), Option(rs.getString("categoria")).getOrElse("Temp"), Option(rs.getString("club_escudo_url")).getOrElse(""), Option(rs.getString("foto_jugador_url")).getOrElse(""), rs.getInt("pj"), rs.getInt("gc"), 0, rs.getDouble("media").toInt) } } finally {conn.close()}; l }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE I — COMPARATIVA ENTRE TEMPORADAS
  // ─────────────────────────────────────────────────────────────────────────────
  def getSeasonsForSelector(): List[(Int, String)] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT id, COALESCE(nombre, categoria, 'Temporada') as label FROM seasons ORDER BY id DESC")
      var l = List[(Int, String)]()
      while (rs.next()) l = l :+ (rs.getInt("id"), fixEncoding(rs.getString("label")))
      l
    } finally { conn.close() }
  }

  private def getTemporadaStats(conn: Connection, seasonId: Int): Map[String, Any] = {
    val rsS = conn.prepareStatement(
      "SELECT COALESCE(nombre, categoria, 'Temporada') as label, fecha_inicio, fecha_fin, " +
      "stat_div, stat_han, stat_kic, stat_ref, stat_spd, stat_pos FROM seasons WHERE id = ?")
    rsS.setInt(1, seasonId)
    val rs = rsS.executeQuery()
    if (!rs.next()) return Map.empty[String, Any]

    val label = fixEncoding(rs.getString("label"))
    val fechaInicio = Option(rs.getDate("fecha_inicio")).map(_.toLocalDate).getOrElse(LocalDate.of(2000,1,1))
    val fechaFin = Option(rs.getDate("fecha_fin")).map(_.toLocalDate).getOrElse(LocalDate.now())
    val (div, han, kic, ref, spd, pos) = (rs.getDouble("stat_div"), rs.getDouble("stat_han"),
      rs.getDouble("stat_kic"), rs.getDouble("stat_ref"), rs.getDouble("stat_spd"), rs.getDouble("stat_pos"))

    val rsM = conn.prepareStatement(
      "SELECT COUNT(*) as pj, " +
      "COUNT(CASE WHEN goles_favor > goles_contra THEN 1 END) as pg, " +
      "COUNT(CASE WHEN goles_favor = goles_contra THEN 1 END) as pe, " +
      "COUNT(CASE WHEN goles_favor < goles_contra THEN 1 END) as pp, " +
      "COALESCE(SUM(goles_favor),0) as gf, COALESCE(SUM(goles_contra),0) as gc, " +
      "COALESCE(AVG(nota),0) as media, " +
      "COUNT(CASE WHEN goles_contra = 0 THEN 1 END) as pcs, " +
      "COALESCE(MAX(nota),0) as mejor, COALESCE(MIN(CASE WHEN nota > 0 THEN nota END),0) as peor " +
      "FROM matches WHERE season_id = ? AND status='PLAYED'")
    rsM.setInt(1, seasonId)
    val rsMr = rsM.executeQuery()
    rsMr.next()
    val pj = rsMr.getInt("pj"); val pg = rsMr.getInt("pg"); val pe = rsMr.getInt("pe"); val pp = rsMr.getInt("pp")
    val gf = rsMr.getInt("gf"); val gc = rsMr.getInt("gc"); val notaMedia = rsMr.getDouble("media")
    val pcs = rsMr.getInt("pcs"); val mejorNota = rsMr.getDouble("mejor"); val peorNota = rsMr.getDouble("peor")

    // Evolucion mensual de la nota media, indexada por mes relativo desde el inicio de temporada (1..N)
    val rsEvol = conn.prepareStatement(
      "SELECT fecha, nota FROM matches WHERE season_id = ? AND status='PLAYED' ORDER BY fecha ASC")
    rsEvol.setInt(1, seasonId)
    val rsEvolR = rsEvol.executeQuery()
    var porMesRel = scala.collection.mutable.Map[Int, List[Double]]().withDefaultValue(Nil)
    while (rsEvolR.next()) {
      val f = rsEvolR.getDate("fecha").toLocalDate
      val mesRel = (java.time.Period.between(fechaInicio.withDayOfMonth(1), f.withDayOfMonth(1)).toTotalMonths + 1).toInt
      if (mesRel >= 1) porMesRel(mesRel) = porMesRel(mesRel) :+ rsEvolR.getDouble("nota")
    }
    val evolMensual = porMesRel.toList.sortBy(_._1).map { case (mes, notas) => (mes, notas.sum / notas.size) }

    // Horas de practica deliberada dentro del rango de la temporada
    var sesiones = List[(Double, Double)]() // (horasBrutas, horasPonderadas)
    val rsT = conn.prepareStatement(
      "SELECT tipo FROM trainings WHERE fecha >= ? AND fecha <= ?")
    rsT.setDate(1, Date.valueOf(fechaInicio)); rsT.setDate(2, Date.valueOf(fechaFin))
    val rsTr = rsT.executeQuery()
    while (rsTr.next()) {
      val peso = pesoTipoPractica(Option(rsTr.getString("tipo")).getOrElse(""))
      sesiones = sesiones :+ (horasPorSesionTrainingDefault, horasPorSesionTrainingDefault * peso)
    }
    val rsMh = conn.prepareStatement(
      "SELECT minutos FROM matches WHERE season_id = ? AND status='PLAYED' AND minutos > 0")
    rsMh.setInt(1, seasonId)
    val rsMhR = rsMh.executeQuery()
    while (rsMhR.next()) {
      val h = rsMhR.getInt("minutos") / 60.0
      sesiones = sesiones :+ (h, h * pesoTipoPractica("Partido"))
    }
    val horasPractica = sesiones.map(_._2).sum

    // Lesiones dentro del rango de la temporada
    val rsL = conn.prepareStatement("SELECT COUNT(*) as c FROM injuries WHERE fecha_inicio >= ? AND fecha_inicio <= ?")
    rsL.setDate(1, Date.valueOf(fechaInicio)); rsL.setDate(2, Date.valueOf(fechaFin))
    val rsLr = rsL.executeQuery()
    val lesiones = if (rsLr.next()) rsLr.getInt("c") else 0

    // Skills de portero conseguidas dentro del rango de la temporada
    val rsSk = conn.prepareStatement(
      "SELECT COUNT(*) as c FROM goalkeeper_skills WHERE conseguido = TRUE AND fecha_conseguido >= ? AND fecha_conseguido <= ?")
    rsSk.setDate(1, Date.valueOf(fechaInicio)); rsSk.setDate(2, Date.valueOf(fechaFin))
    val rsSkr = rsSk.executeQuery()
    val skillsConseguidas = if (rsSkr.next()) rsSkr.getInt("c") else 0

    Map(
      "label" -> label, "pj" -> pj, "pg" -> pg, "pe" -> pe, "pp" -> pp, "gf" -> gf, "gc" -> gc,
      "notaMedia" -> notaMedia, "porteriasCero" -> pcs, "mejorNota" -> mejorNota, "peorNota" -> peorNota,
      "div" -> div, "han" -> han, "kic" -> kic, "ref" -> ref, "spd" -> spd, "pos" -> pos,
      "horasPractica" -> horasPractica, "lesiones" -> lesiones, "skillsConseguidas" -> skillsConseguidas,
      "evolMensual" -> evolMensual
    )
  }

  def getTemporadasComparativa(id1: Int, id2: Int): Map[String, Any] = {
    val conn = getConnection()
    try {
      Map("season1" -> getTemporadaStats(conn, id1), "season2" -> getTemporadaStats(conn, id2))
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE F — COMPARATIVA LONGITUDINAL CON SI MISMO (activa con >=2 temporadas)
  // ─────────────────────────────────────────────────────────────────────────────
  // SQL/matematicas puras — sin Gemini.
  def getComparativaLongitudinal(): Map[String, Any] = {
    val conn = getConnection()
    try {
      val temporadas = getSeasonsForSelector().reverse // cronologico ascendente
      if (temporadas.size < 2) return Map("suficiente" -> false, "n" -> temporadas.size)

      val cognitivoTests = getCognitivoTests() // (fecha, indice) — pequeno, se filtra en memoria por temporada

      val datos = temporadas.map { case (id, label) =>
        val stats = getTemporadaStats(conn, id)
        val pj = stats.getOrElse("pj", 0).asInstanceOf[Int]
        val pcs = stats.getOrElse("porteriasCero", 0).asInstanceOf[Int]
        val gc = stats.getOrElse("gc", 0).asInstanceOf[Int]
        val pctPorteriasCero = if (pj > 0) pcs * 100.0 / pj else 0.0
        val gcPorPartido = if (pj > 0) gc.toDouble / pj else 0.0
        val futMedia = Seq("div","han","kic","ref","spd","pos").map(k => stats.getOrElse(k, 0.0).asInstanceOf[Double]).sum / 6.0

        val rsFechas = conn.prepareStatement("SELECT fecha_inicio, fecha_fin FROM seasons WHERE id=?")
        rsFechas.setInt(1, id)
        val rf = rsFechas.executeQuery()
        val (fechaIni, fechaFin) = if (rf.next()) (Option(rf.getDate("fecha_inicio")), Option(rf.getDate("fecha_fin"))) else (None, None)

        val acwrMedio: Option[Double] = (fechaIni, fechaFin) match {
          case (Some(a), Some(b)) =>
            val ps = conn.prepareStatement("SELECT AVG(acwr_score) as m, COUNT(*) as n FROM forma_diaria WHERE fecha >= ? AND fecha <= ?")
            ps.setDate(1, a); ps.setDate(2, b)
            val r = ps.executeQuery()
            if (r.next() && r.getInt("n") > 0) Some(r.getDouble("m")) else None
          case _ => None
        }

        val indiceCognitivoMedio: Option[Double] = (fechaIni, fechaFin) match {
          case (Some(a), Some(b)) =>
            val enRango = cognitivoTests.filter { t =>
              val f = t("fecha").asInstanceOf[String]
              f >= a.toString && f <= b.toString
            }
            if (enRango.nonEmpty) Some(enRango.map(_("indice").asInstanceOf[Double]).sum / enRango.size) else None
          case _ => None
        }

        // Velocidad de aprendizaje: skills conseguidas por mes de temporada
        val mesesTemporada = (fechaIni, fechaFin) match {
          case (Some(a), Some(b)) => math.max(1, java.time.Period.between(a.toLocalDate, b.toLocalDate).toTotalMonths.toInt)
          case _ => 1
        }
        val skillsPorMes = stats.getOrElse("skillsConseguidas", 0).asInstanceOf[Int].toDouble / mesesTemporada

        Map(
          "temporada" -> label,
          "notaMedia" -> stats.getOrElse("notaMedia", 0.0).asInstanceOf[Double],
          "gcPorPartido" -> gcPorPartido,
          "pctPorteriasCero" -> pctPorteriasCero,
          "futMedia" -> futMedia,
          "skillsPorMes" -> skillsPorMes,
          "horasPractica" -> stats.getOrElse("horasPractica", 0.0).asInstanceOf[Double],
          "acwrMedio" -> acwrMedio,
          "indiceCognitivoMedio" -> indiceCognitivoMedio
        )
      }

      Map("suficiente" -> true, "n" -> temporadas.size, "temporadas" -> datos)
    } finally { conn.close() }
  }
  def saveRivalInfo(nombre: String, estilo: String, claves: String, notas: String): Unit = { val conn = getConnection(); try { conn.createStatement().executeUpdate(s"DELETE FROM rivals WHERE LOWER(nombre) = LOWER('${fixEncoding(nombre)}')"); val ps = conn.prepareStatement("INSERT INTO rivals (nombre, estilo_juego, jugadores_clave, notas_scouting) VALUES (?,?,?,?)"); ps.setString(1, fixEncoding(nombre)); ps.setString(2, estilo); ps.setString(3, fixEncoding(claves)); ps.setString(4, fixEncoding(notas)); ps.executeUpdate() } finally { conn.close() } }
  def getRivalInfo(nombre: String): Option[RivalInfo] = { var r: Option[RivalInfo]=None; val conn=getConnection(); try{ val ps=conn.prepareStatement("SELECT * FROM rivals WHERE LOWER(nombre)=LOWER(?)"); ps.setString(1,fixEncoding(nombre)); val rs=ps.executeQuery(); if(rs.next()) r=Some(RivalInfo(rs.getString("nombre"), rs.getString("estilo_juego"), rs.getString("jugadores_clave"), rs.getString("notas_scouting"))) } finally {conn.close()}; r }

  // BLOQUE F: estilo/claves/notas de scouting de un rival para las flash-cards (busqueda parcial, como getFlashCardData)
  def getRivalScoutingNotas(rival: String): Map[String, String] = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("SELECT estilo_juego, jugadores_clave, notas_scouting FROM rivals WHERE LOWER(nombre) LIKE LOWER(?)")
      ps.setString(1, s"%${fixEncoding(rival)}%")
      val rs = ps.executeQuery()
      if (rs.next()) Map(
        "estilo" -> Option(rs.getString("estilo_juego")).getOrElse(""),
        "claves" -> Option(rs.getString("jugadores_clave")).getOrElse(""),
        "notas"  -> Option(rs.getString("notas_scouting")).getOrElse("")
      ) else Map.empty[String, String]
    } finally { conn.close() }
  }

  // BLOQUE F: historial completo (no limitado a 5) contra un rival — PJ, GF, GC, nota media
  def getRivalHistorialCompleto(rival: String): Map[String, Any] = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement(
        "SELECT COUNT(*) as pj, COALESCE(SUM(goles_favor),0) as gf, COALESCE(SUM(goles_contra),0) as gc, COALESCE(AVG(nota),0) as nota_media " +
        "FROM matches WHERE LOWER(rival) LIKE LOWER(?) AND status='PLAYED'")
      ps.setString(1, s"%${fixEncoding(rival)}%")
      val rs = ps.executeQuery()
      if (rs.next()) Map(
        "pj" -> rs.getInt("pj"), "gf" -> rs.getInt("gf"), "gc" -> rs.getInt("gc"),
        "notaMedia" -> rs.getDouble("nota_media")
      ) else Map("pj" -> 0, "gf" -> 0, "gc" -> 0, "notaMedia" -> 0.0)
    } finally { conn.close() }
  }
  def addNewDrill(nombre: String, desc: String): Unit = { val conn = getConnection(); try { val ps = conn.prepareStatement("INSERT INTO drills (nombre, descripcion, sesiones_objetivo, sesiones_actuales, activo) VALUES (?, ?, 10, 0, TRUE)"); ps.setString(1, fixEncoding(nombre)); ps.setString(2, fixEncoding(desc)); ps.executeUpdate() } finally { conn.close() } }
  def getOMSPercents(): (Double, Double, Double, Double, Double, Double) = {
    val card = getLatestCardData()
    val edad = calcularEdadExacta(card.fechaNacimiento)

    // Tabla Maestra OMS 5-18 anos (Percentiles 15, 50, 85)
    // Estructura: Edad -> (H50, H15, H85, W50, W15, W85)
    val tablaOMS = Map(
      5  -> (110.0, 105.3, 114.7, 18.3, 16.2, 21.0),
      6  -> (116.0, 111.0, 121.0, 20.5, 18.0, 24.0),
      7  -> (122.1, 116.8, 127.4, 22.9, 19.8, 27.2),
      8  -> (127.7, 122.1, 133.3, 25.4, 21.9, 30.5),
      9  -> (133.3, 127.3, 139.3, 28.1, 24.1, 34.1),
      10 -> (138.4, 132.3, 144.5, 31.2, 26.7, 38.2),
      11 -> (143.5, 137.1, 149.9, 34.6, 29.5, 43.1),
      12 -> (149.1, 142.2, 156.0, 38.6, 32.7, 48.9),
      13 -> (156.0, 148.5, 163.5, 43.5, 36.5, 55.5),
      14 -> (163.2, 155.0, 171.5, 49.3, 41.0, 63.0),
      15 -> (169.0, 161.0, 177.0, 55.0, 46.5, 70.0),
      16 -> (173.0, 165.0, 181.0, 60.5, 51.5, 76.5),
      17 -> (175.2, 167.0, 183.5, 64.5, 55.0, 81.5),
      18 -> (176.0, 168.0, 184.0, 67.0, 57.0, 84.0)
    )

    tablaOMS.getOrElse(edad, (176.0, 168.0, 184.0, 67.0, 57.0, 84.0))
  }
  def getActiveDrills(): List[Drill] = { var l=List[Drill](); val conn=getConnection(); try{ val rs=conn.createStatement().executeQuery("SELECT * FROM drills WHERE activo=TRUE ORDER BY id DESC"); while(rs.next()) l=l:+Drill(rs.getInt("id"), rs.getString("nombre"), Option(rs.getString("descripcion")).getOrElse(""), rs.getInt("sesiones_actuales"), rs.getInt("sesiones_objetivo")) } catch {case _:Exception=>} finally {conn.close()}; l }
  def progressDrills(): Unit = { val conn=getConnection(); try{ conn.createStatement().executeUpdate("UPDATE drills SET sesiones_actuales = sesiones_actuales + 1 WHERE activo = TRUE"); conn.createStatement().executeUpdate("UPDATE drills SET activo = FALSE WHERE sesiones_actuales >= sesiones_objetivo") } finally {conn.close()} }
  def importMatchesCSV(csvData: String): String = {
    var count = 0
    val lines = csvData.split("\n").map(_.trim).filter(_.nonEmpty)
    val dataLines = if (lines.headOption.exists(_.toLowerCase.contains("rival"))) lines.tail else lines
    val today = LocalDate.now().toString

    dataLines.foreach { line =>
      try {
        val p = line.split(",").map(_.trim)
        if (p.length >= 6) {
          val rival = fixEncoding(p(0))
          val gf = p(1).toInt
          val gc = p(2).toInt
          val min = p(3).toInt
          val nota = p(4).toDouble
          val paradas = p(5).toInt
          val clima = if(p.length > 6) p(6) else "Sol"
          val notas = if(p.length > 7) fixEncoding(p(7)) else "Importado"
          val reaccion = if(p.length > 8) fixEncoding(p(8)) else ""

          val c = getLatestCardData()
          val n = StatsCalculator.calculateGrowth(c, min, gc, nota, paradas, 0,0,0,0)
          updateStats(n)

          val m = (n.divRaw * 0.2 + n.hanRaw * 0.2 + n.refRaw * 0.2 + n.posRaw * 0.2 + n.spdRaw * 0.05 + n.kicRaw * 0.15)

          // AQUI ESTABA EL ERROR: Faltaba el ultimo argumento "" para mapaCampo
          logMatch(rival, gf, gc, min, nota, m, paradas, "", "", "", 0, 0, 0, clima, "-", 20, notas, "", reaccion, today, "LIGA", 0,0,0,0, "")

          count += 1
        }
      } catch {
        case e: Exception => println(s"Error import line: $line")
      }
    }
    s"Importados $count partidos"
  }
  def importCalendarCSV(csvData: String): String = { var count=0; val lines=csvData.split("\n").map(_.trim).filter(_.nonEmpty); val conn=getConnection(); try{ val rs=conn.createStatement().executeQuery("SELECT MAX(id) as id FROM seasons"); if(rs.next()){ val sId=rs.getInt("id"); val ps=conn.prepareStatement("INSERT INTO matches (season_id, fecha, rival, tipo_partido, status, goles_favor, goles_contra, minutos, nota, paradas) VALUES (?, ?, ?, ?, 'SCHEDULED', 0, 0, 0, 0, 0)"); lines.foreach { l => try { val p=l.split(",").map(_.trim); if(p.length>=2){ ps.setInt(1,sId); ps.setDate(2,Date.valueOf(p(0))); ps.setString(3,fixEncoding(p(1))); ps.setString(4,if(p.length>2) p(2).toUpperCase else "LIGA"); ps.executeUpdate(); count+=1 } } catch {case _:Exception=>} } } } finally {conn.close()}; s"Importados $count eventos." }
  def importWellnessCSV(csvData: String): String = { var count=0; val lines=csvData.split("\n").map(_.trim).filter(_.nonEmpty); val dataLines=if(lines.headOption.exists(_.toLowerCase.contains("sueno"))) lines.tail else lines; dataLines.foreach { line => try { val p=line.split(",").map(_.trim); if(p.length>=5){ logWellness(p(0).toInt, p(1).toDouble, p(2).toInt, p(3).toInt, if(p.length>4) p(4) else "", 0, 0.0, if(p.length>5) p(5).toInt else 3, "", "DISPONIBLE"); count+=1 } } catch {case _:Exception=>} }; s"Importados $count registros bio." }
  def logGrowth(altura: Double, peso: Double, tallaSentadoCm: Option[Double] = None, longitudPiernaCm: Option[Double] = None,
                kgMusculo: Option[Double] = None, kgMasaOsea: Option[Double] = None): Unit = {
    val conn = getConnection()
    try {
      var velocity = 0.0
      val rsLast = conn.createStatement().executeQuery("SELECT altura FROM physical_growth ORDER BY fecha DESC LIMIT 1")
      if (rsLast.next()) { val lastHeight = rsLast.getDouble("altura"); if (altura > lastHeight) velocity = altura - lastHeight }
      val ps = conn.prepareStatement("INSERT INTO physical_growth (altura, peso, velocidad_crecimiento, talla_sentado_cm, longitud_pierna_cm, kg_musculo, kg_masa_osea) VALUES (?, ?, ?, ?, ?, ?, ?)")
      ps.setDouble(1, altura); ps.setDouble(2, peso); ps.setDouble(3, velocity)
      tallaSentadoCm match { case Some(v) => ps.setDouble(4, v); case None => ps.setNull(4, java.sql.Types.DOUBLE) }
      longitudPiernaCm match { case Some(v) => ps.setDouble(5, v); case None => ps.setNull(5, java.sql.Types.DOUBLE) }
      kgMusculo match { case Some(v) => ps.setDouble(6, v); case None => ps.setNull(6, java.sql.Types.DOUBLE) }
      kgMasaOsea match { case Some(v) => ps.setDouble(7, v); case None => ps.setNull(7, java.sql.Types.DOUBLE) }
      ps.executeUpdate()
    } finally { conn.close() }
  }
  def getGrowthHistory(): String = {
    var l=List[String](); var dAlt=List[Double](); var dPeso=List[Double]()
    val conn=getConnection()
    try {
      // Extraemos altura y peso de la tabla physical_growth
      val rs=conn.createStatement().executeQuery("SELECT TO_CHAR(fecha, 'MM-DD') as f, altura, peso FROM physical_growth ORDER BY fecha ASC LIMIT 12")
      while(rs.next()){
        l = l :+ s"'${rs.getString("f")}'"
        dAlt = dAlt :+ rs.getDouble("altura")
        dPeso = dPeso :+ rs.getDouble("peso")
      }
    } finally {conn.close()}
    // El JSON debe coincidir con lo que el JavaScript espera
    s"""{ "labels": [${l.mkString(",")}], "altura": [${dAlt.mkString(",")}], "peso": [${dPeso.mkString(",")}] }"""
  }
  def addVideoTag(matchId: Int, min: Int, sec: Int, tipo: String, desc: String): Unit = { val conn=getConnection(); try{ val ps=conn.prepareStatement("INSERT INTO video_tags (match_id, minuto, segundo, tipo, descripcion) VALUES (?,?,?,?,?)"); ps.setInt(1, matchId); ps.setInt(2, min); ps.setInt(3, sec); ps.setString(4, tipo); ps.setString(5, fixEncoding(desc)); ps.executeUpdate() } finally {conn.close()} }
  def getVideoTags(matchId: Int): List[VideoTag] = { var l=List[VideoTag](); val conn=getConnection(); try{ val rs=conn.createStatement().executeQuery(s"SELECT * FROM video_tags WHERE match_id=$matchId ORDER BY minuto ASC, segundo ASC"); while(rs.next()) l=l:+VideoTag(rs.getInt("id"), rs.getInt("match_id"), rs.getInt("minuto"), rs.getInt("segundo"), rs.getString("tipo"), rs.getString("descripcion")) } finally {conn.close()}; l }
  def deleteVideoTag(id: Int): Unit = { val conn=getConnection(); try{ conn.createStatement().executeUpdate(s"DELETE FROM video_tags WHERE id=$id") } finally {conn.close()} }

  def getVideotecaClips(tipoFiltro: String = ""): List[VideoClip] = {
    var clips = List[VideoClip]()
    val conn = getConnection()
    try {
      val where = if (tipoFiltro.nonEmpty) s"AND vt.tipo = '$tipoFiltro'" else ""
      val sql = s"""
        SELECT vt.id, vt.match_id, m.rival, m.fecha, m.video_url AS video,
               vt.minuto, vt.segundo, vt.tipo
        FROM video_tags vt
        JOIN matches m ON vt.match_id = m.id
        WHERE m.video_url IS NOT NULL AND m.video_url != '' $where
        ORDER BY m.fecha DESC, vt.minuto ASC, vt.segundo ASC
      """
      val rs = conn.createStatement().executeQuery(sql)
      while (rs.next()) {
        clips = clips :+ VideoClip(
          rs.getInt("id"), rs.getInt("match_id"),
          rs.getString("rival"), rs.getString("fecha"),
          rs.getString("video"),
          rs.getInt("minuto"), rs.getInt("segundo"),
          rs.getString("tipo")
        )
      }
    } finally { conn.close() }
    clips
  }
  def logPenalty(rival: String, zTiro: String, zSalto: String, esGol: Boolean): Unit = { val conn = getConnection(); try { val ps = conn.prepareStatement("INSERT INTO penalties (rival, zona_tiro, zona_salto, es_gol) VALUES (?, ?, ?, ?)"); ps.setString(1, fixEncoding(rival)); ps.setString(2, zTiro); ps.setString(3, zSalto); ps.setBoolean(4, esGol); ps.executeUpdate() } finally { conn.close() } }
  def getPenaltyStats(): List[PenaltyStat] = { val l = scala.collection.mutable.ListBuffer[PenaltyStat](); val conn = getConnection(); try { val rs = conn.createStatement().executeQuery("SELECT zona_tiro, COUNT(*) as total, SUM(CASE WHEN es_gol THEN 1 ELSE 0 END) as goles FROM penalties GROUP BY zona_tiro"); while(rs.next()) l += PenaltyStat(rs.getString("zona_tiro"), rs.getInt("total"), rs.getInt("goles")) } finally { conn.close() }; l.toList }
  def addNewGear(nombre: String, tipo: String, vida: Int, img: String): Unit = { val conn=getConnection(); try { val r=conn.prepareStatement("UPDATE gear SET activo=FALSE WHERE tipo=? AND activo=TRUE"); r.setString(1,tipo); r.executeUpdate(); val a=conn.prepareStatement("INSERT INTO gear (nombre, tipo, vida_util_estimada, usos_actuales, activo, imagen_url) VALUES (?,?,?,0,TRUE, ?)"); a.setString(1,fixEncoding(nombre)); a.setString(2,tipo); a.setInt(3,vida); a.setString(4, img); a.executeUpdate() } finally { conn.close() } }
  def getActiveGear(): List[GearItem] = { var l=List[GearItem](); val conn=getConnection(); try { val rs=conn.createStatement().executeQuery("SELECT * FROM gear WHERE activo = TRUE ORDER BY tipo DESC"); while(rs.next()) { val (u,max)=(rs.getInt("usos_actuales"),rs.getInt("vida_util_estimada")); l=l:+GearItem(rs.getInt("id"),rs.getString("nombre"),rs.getString("tipo"),u,max,if(max>0 && u.toDouble/max > 0.9) "Critico" else "Optimo", Option(rs.getString("imagen_url")).getOrElse("")) } } finally { conn.close() }; l }
  def logWellness(sueno: Int, horas: Double, energia: Int, dolor: Int, zona: String, altura: Int, peso: Double, animo: Int, notas: String, estadoFisico: String,
                   suenoProfundoMin: Option[Int] = None, suenoLigeroMin: Option[Int] = None, suenoDespiertoMin: Option[Int] = None,
                   tallaSentadoCm: Option[Double] = None, longitudPiernaCm: Option[Double] = None,
                   kgMusculo: Option[Double] = None, kgMasaOsea: Option[Double] = None,
                   fcReposo: Option[Int] = None,
                   somnolencia: Option[Int] = None, dolorMuscular: Option[Int] = None): Unit = {
    val conn=getConnection()
    try {
      // Upsert por fecha: wellness.fecha es unica (ver initDB), asi que guardar dos
      // veces el mismo dia actualiza la fila en vez de violar la restriccion.
      // fc_reposo se conserva si esta llamada no trae uno nuevo, para no borrar
      // una medicion ya importada desde la captura del smartwatch.
      val s=conn.prepareStatement("""
        INSERT INTO wellness (fecha, sueno, horas_sueno, energia, dolor, zona_dolor, altura, peso, animo, notas_conducta, estado_fisico, sueno_profundo_min, sueno_ligero_min, sueno_despierto_min, fc_reposo, somnolencia, dolor_muscular)
        VALUES (CURRENT_DATE, ?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
        ON CONFLICT (fecha) DO UPDATE SET
          sueno = EXCLUDED.sueno, horas_sueno = EXCLUDED.horas_sueno, energia = EXCLUDED.energia, dolor = EXCLUDED.dolor,
          zona_dolor = EXCLUDED.zona_dolor, altura = EXCLUDED.altura, peso = EXCLUDED.peso, animo = EXCLUDED.animo,
          notas_conducta = EXCLUDED.notas_conducta, estado_fisico = EXCLUDED.estado_fisico,
          sueno_profundo_min = EXCLUDED.sueno_profundo_min, sueno_ligero_min = EXCLUDED.sueno_ligero_min,
          sueno_despierto_min = EXCLUDED.sueno_despierto_min,
          fc_reposo = COALESCE(EXCLUDED.fc_reposo, wellness.fc_reposo),
          somnolencia = EXCLUDED.somnolencia, dolor_muscular = EXCLUDED.dolor_muscular
      """)
      s.setInt(1,sueno); s.setDouble(2, horas); s.setInt(3,energia); s.setInt(4,dolor); s.setString(5,fixEncoding(zona)); s.setInt(6, altura); s.setDouble(7, peso); s.setInt(8, animo); s.setString(9, fixEncoding(notas)); s.setString(10, estadoFisico)
      def setOptInt(idx: Int, v: Option[Int]): Unit = v match { case Some(x) => s.setInt(idx, x); case None => s.setNull(idx, java.sql.Types.INTEGER) }
      setOptInt(11, suenoProfundoMin); setOptInt(12, suenoLigeroMin); setOptInt(13, suenoDespiertoMin)
      setOptInt(14, fcReposo); setOptInt(15, somnolencia); setOptInt(16, dolorMuscular)
      s.executeUpdate()
      if(altura > 0 && peso > 0) logGrowth(altura.toDouble, peso, tallaSentadoCm, longitudPiernaCm, kgMusculo, kgMasaOsea)
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────
  // Importacion de FC en reposo por captura de pantalla del smartwatch
  // ─────────────────────────────────────────────────────────────────────────
  def importFcReposoFromImage(base64Content: String, mimeType: String): Map[String, Any] = {
    val prompt = "Analiza esta captura de pantalla de una app de smartwatch que muestra mediciones de frecuencia cardíaca. Extrae TODAS las mediciones visibles. Para cada medición devuelve la fecha en formato YYYY-MM-DD y el valor BPM como número entero. Si la fecha aparece como '21 sep' o '09-21' infiere el año como el año actual. Ignora mediciones con BPM=0, mayor de 200 o menor de 30. Devuelve ÚNICAMENTE un JSON válido sin backticks ni texto adicional: [{\"fecha\": \"YYYY-MM-DD\", \"bpm\": 76}, ...]. Si no puedes leer ninguna medición devuelve []."

    val respuesta = AIProvider.ask(prompt, Some((mimeType, base64Content)), bypassCache = true)
    val cleaned = respuesta.replace("```json", "").replace("```", "").trim
    val mediciones = try { ujson.read(cleaned).arr } catch { case _: Exception => scala.collection.mutable.ArrayBuffer.empty[ujson.Value] }

    var importados = 0
    var yaExistian = 0
    val detalle = scala.collection.mutable.ListBuffer[Map[String, Any]]()

    val conn = getConnection()
    try {
      mediciones.foreach { m =>
        val fechaOpt = try { Some(java.time.LocalDate.parse(m("fecha").str).toString) } catch { case _: Exception => None }
        val bpmOpt   = try { Some(m("bpm").num.toInt) } catch { case _: Exception => None }
        (fechaOpt, bpmOpt) match {
          case (Some(fecha), Some(bpm)) if bpm >= 30 && bpm <= 200 =>
            val ps = conn.prepareStatement(
              """INSERT INTO wellness (fecha, fc_reposo) VALUES (?::date, ?)
                 ON CONFLICT (fecha) DO UPDATE SET fc_reposo = EXCLUDED.fc_reposo
                 WHERE wellness.fc_reposo IS NULL"""
            )
            ps.setString(1, fecha)
            ps.setInt(2, bpm)
            val actualizada = ps.executeUpdate() > 0
            if (actualizada) { importados += 1; detalle += Map("fecha" -> fecha, "bpm" -> bpm, "estado" -> "importado") }
            else { yaExistian += 1; detalle += Map("fecha" -> fecha, "bpm" -> bpm, "estado" -> "existia") }
          case _ => ()
        }
      }
    } finally { conn.close() }

    Map("importados" -> importados, "yaExistian" -> yaExistian, "detalle" -> detalle.toList)
  }
  // BLOQUE PROBLEMA 2/3: fecha editable (por defecto hoy) y tipoAusencia opcional (registro de "no asistio")
  def logTraining(tipo: String, foco: String, rpe: Int, calidad: Int, atencion: Int, rutina: String, feedbackEntrenador: String = "",
                   fbDistancia: Option[Double] = None, fbAltaIntensidad: Option[Int] = None, fbSprintMax: Option[Double] = None,
                   fbPctActividad: Option[Int] = None, fbTiempoActivo: Option[Int] = None,
                   fbAceleraciones: Option[Int] = None, fbDesaceleraciones: Option[Int] = None,
                   fecha: String = "", tipoAusencia: Option[String] = None, duracionMin: Option[Int] = None,
                   rpeHector: Option[Int] = None): Int = {
    val conn=getConnection()
    try {
      val s=conn.prepareStatement("INSERT INTO trainings (tipo, foco, rpe, calidad, atencion, rutina_detalle, feedback_entrenador, fb_distancia, fb_alta_intensidad, fb_sprint_max, fb_pct_actividad, fb_tiempo_activo, fb_aceleraciones, fb_desaceleraciones, fecha, tipo_ausencia, duracion_min, rpe_hector) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?::date,?,?,?) RETURNING id")
      s.setString(1,tipo); s.setString(2,fixEncoding(foco)); s.setInt(3,rpe); s.setInt(4,calidad); s.setInt(5, atencion); s.setString(6,fixEncoding(rutina))
      if (feedbackEntrenador.nonEmpty) s.setString(7, fixEncoding(feedbackEntrenador)) else s.setNull(7, java.sql.Types.VARCHAR)
      def setOptDouble(idx: Int, v: Option[Double]): Unit = v match { case Some(x) => s.setDouble(idx, x); case None => s.setNull(idx, java.sql.Types.DOUBLE) }
      def setOptInt(idx: Int, v: Option[Int]): Unit = v match { case Some(x) => s.setInt(idx, x); case None => s.setNull(idx, java.sql.Types.INTEGER) }
      // Solo se guardan los datos Footbar si se ha rellenado la distancia
      if (fbDistancia.isDefined) {
        setOptDouble(8, fbDistancia); setOptInt(9, fbAltaIntensidad); setOptDouble(10, fbSprintMax)
        setOptInt(11, fbPctActividad); setOptInt(12, fbTiempoActivo); setOptInt(13, fbAceleraciones); setOptInt(14, fbDesaceleraciones)
      } else {
        s.setNull(8, java.sql.Types.DOUBLE); s.setNull(9, java.sql.Types.INTEGER); s.setNull(10, java.sql.Types.DOUBLE)
        s.setNull(11, java.sql.Types.INTEGER); s.setNull(12, java.sql.Types.INTEGER); s.setNull(13, java.sql.Types.INTEGER); s.setNull(14, java.sql.Types.INTEGER)
      }
      s.setString(15, if (fecha.nonEmpty) fecha else LocalDate.now().toString)
      tipoAusencia match {
        case Some(t) if t.nonEmpty => s.setString(16, t)
        case _ => s.setNull(16, java.sql.Types.VARCHAR)
      }
      setOptInt(17, duracionMin); setOptInt(18, rpeHector.filter(v => v >= 1 && v <= 5))
      val rsId = s.executeQuery()
      val id = if (rsId.next()) rsId.getInt("id") else -1
      conn.createStatement().executeUpdate("UPDATE gear SET usos_actuales = usos_actuales + 1 WHERE activo = TRUE")
      if (tipo.contains("Papa")) progressDrills()
      id
    } finally { conn.close() }
  }

  def getUltimoFeedbackEntrenador(): Option[String] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT feedback_entrenador, fecha FROM trainings WHERE tipo='Academia' AND feedback_entrenador IS NOT NULL AND feedback_entrenador != '' AND fecha >= CURRENT_DATE - 7 ORDER BY fecha DESC LIMIT 1")
      if (rs.next()) Some(rs.getString("feedback_entrenador")) else None
    } finally { conn.close() }
  }
  // --- EN: DatabaseManager.scala ---

  def getSmartInsights(): String = {
    val conn = getConnection()
    try {
      val sb = new StringBuilder()

      // 1. DETECTOR "TORPEZA DEL ESTIRON" (Crecimiento Rapido + Bajada Coordinacion)
      val rsGrowth = conn.createStatement().executeQuery("SELECT velocidad_crecimiento FROM physical_growth ORDER BY fecha DESC LIMIT 1")
      val growthSpeed = if(rsGrowth.next()) rsGrowth.getDouble("velocidad_crecimiento") else 0.0

      if (growthSpeed > 0.5) { // Si ha crecido mas de 0.5cm recientemente
        val rsTech = conn.createStatement().executeQuery("SELECT coordinacion FROM technical_reviews ORDER BY fecha DESC LIMIT 2")
        if (rsTech.next()) {
          val currCoord = rsTech.getInt("coordinacion")
          if (rsTech.next()) {
            val prevCoord = rsTech.getInt("coordinacion")
            if (currCoord < prevCoord) {
              sb.append(s"⚠️ ALERTA BIO-MECÁNICA: Crecimiento acelerado detectado (+${growthSpeed}cm) coincidiendo con bajada de coordinación. Riesgo de 'Torpeza del Estirón'. Recomendación: simplificar tareas técnicas y trabajar propiocepción.\n")
            }
          }
        }
      }

      // BLOQUE B7: la deteccion de dolor/malestar SOLO puede venir de datos estructurados
      // (tabla injuries y wellness.energia) — nunca de wellness.dolor cruzado con el tipo/foco
      // de entrenamiento, que generaba falsos positivos y confundia correlacion con causalidad.

      if (sb.isEmpty) "Sin anomalías biométricas detectadas hoy." else sb.toString().trim

    } catch {
      case e: Exception => "Error calculando insights."
    } finally {
      conn.close()
    }
  }
  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE B — ESTRUCTURA SEMANAL FIJA DE HECTOR (Elite exclusivamente)
  // ─────────────────────────────────────────────────────────────────────────────
  // Mapea el tipo_sesion de weekly_structure al patron usado en trainings.tipo
  private def patronTipoSesion(tipoSesion: String): String = tipoSesion match {
    case "JUDO"     => "%judo%"
    case "ACADEMIA" => "%academia%"
    case "EQUIPO"   => "%club%"
    case other      => s"%${other.toLowerCase}%"
  }

  private def sesionRegistrada(conn: Connection, fecha: LocalDate, tipoSesion: String): Boolean = {
    if (tipoSesion == "PARTIDO") {
      val ps = conn.prepareStatement("SELECT COUNT(*) as c FROM matches WHERE status='PLAYED' AND fecha = ?")
      ps.setDate(1, Date.valueOf(fecha))
      val rs = ps.executeQuery(); rs.next() && rs.getInt("c") > 0
    } else {
      val ps = conn.prepareStatement("SELECT COUNT(*) as c FROM trainings WHERE fecha = ? AND LOWER(tipo) LIKE ?")
      ps.setDate(1, Date.valueOf(fecha)); ps.setString(2, patronTipoSesion(tipoSesion))
      val rs = ps.executeQuery(); rs.next() && rs.getInt("c") > 0
    }
  }

  private def etiquetaTipoSesion(tipoSesion: String): String = tipoSesion match {
    case "JUDO"     => "🥋 Judo"
    case "ACADEMIA" => "🥅 Academia"
    case "EQUIPO"   => "⚽ Entreno equipo"
    case "PARTIDO"  => "🏟️ Partido"
    case other      => other
  }

  // Solo sesiones esperadas de los ultimos 3 dias (dentro de la semana actual) sin registro. SQL puro.
  // PROBLEMA 3: una ausencia registrada (tipo_ausencia IS NOT NULL) es una fila real de trainings
  // para esa fecha+tipo, asi que sesionRegistrada() ya la cuenta como "registrada" — el aviso
  // desaparece automaticamente, sin necesitar logica adicional aqui.
  def getSemanaIncompleta(): List[String] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("SELECT dia_semana, tipo_sesion, notas FROM weekly_structure WHERE activo = TRUE")
      var estructura = List[(Int, String)]()
      while (rs.next()) estructura = estructura :+ (rs.getInt("dia_semana"), rs.getString("tipo_sesion"))

      val hoy = LocalDate.now()
      var pendientes = List[String]()
      for (i <- 0 to 2) {
        val fecha = hoy.minusDays(i)
        val diaSemana = fecha.getDayOfWeek.getValue // 1=Lunes .. 7=Domingo
        estructura.filter(_._1 == diaSemana).foreach { case (_, tipoSesion) =>
          if (!sesionRegistrada(conn, fecha, tipoSesion)) {
            val diaLabel = fecha.getDayOfWeek.getDisplayName(java.time.format.TextStyle.FULL, new java.util.Locale("es", "ES")).capitalize
            pendientes = pendientes :+ s"${etiquetaTipoSesion(tipoSesion)} del $diaLabel (${fecha.format(java.time.format.DateTimeFormatter.ofPattern("dd/MM"))}) sin registrar"
          }
        }
      }
      pendientes
    } finally { conn.close() }
  }

  // BLOQUE B3: tipo de sesion esperado hoy segun weekly_structure, para pre-rellenar el formulario de entreno.
  // Mapea al value de los <option> ya existentes en BioController (Club/Academia/Judo). PARTIDO no aplica aqui.
  def getTipoSesionHoy(): Option[String] = {
    val conn = getConnection()
    try {
      val diaSemana = LocalDate.now().getDayOfWeek.getValue
      val ps = conn.prepareStatement("SELECT tipo_sesion FROM weekly_structure WHERE dia_semana = ? AND activo = TRUE AND tipo_sesion <> 'PARTIDO' LIMIT 1")
      ps.setInt(1, diaSemana)
      val rs = ps.executeQuery()
      if (!rs.next()) None
      else rs.getString("tipo_sesion") match {
        case "JUDO"     => Some("Judo")
        case "EQUIPO"   => Some("Club")
        case "ACADEMIA" => Some("Academia")
        case _          => None
      }
    } finally { conn.close() }
  }

  // BLOQUE B5: gestion de la estructura semanal desde /settings
  def getWeeklyStructure(): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("SELECT id, dia_semana, tipo_sesion, activo, notas FROM weekly_structure ORDER BY dia_semana ASC, tipo_sesion ASC")
      var l = List[Map[String, Any]]()
      while (rs.next()) l = l :+ Map(
        "id" -> rs.getInt("id"), "diaSemana" -> rs.getInt("dia_semana"), "tipoSesion" -> rs.getString("tipo_sesion"),
        "activo" -> rs.getBoolean("activo"), "notas" -> Option(rs.getString("notas")).getOrElse("")
      )
      l
    } finally { conn.close() }
  }

  def updateWeeklySlot(id: Int, activo: Boolean, diaSemana: Int): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("UPDATE weekly_structure SET activo = ?, dia_semana = ? WHERE id = ?")
      ps.setBoolean(1, activo); ps.setInt(2, diaSemana); ps.setInt(3, id)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  // BLOQUE B4 / PROBLEMA 3: sesiones esperadas segun weekly_structure sin NINGUN registro en
  // trainings/matches Elite, estimadas con RPE=5 (misma escala 60*rpe que el resto de getWorkloads)
  // para no subestimar el ACWR. Las ausencias registradas (tipo_ausencia IS NOT NULL) SI cuentan
  // como "registradas" para sesionRegistrada() — no se anade aqui ninguna carga sintetica para
  // ellas — pero ya contribuyen carga real = 0 a traves de getWorkloads(), porque son una fila
  // normal de trainings con rpe=0. Asi una ausencia planificada (rpe=0, sin carga) se distingue
  // correctamente de una sesion simplemente olvidada (rpe=5 estimado).
  private def sesionesEsperadasNoRegistradas(days: Int): List[Double] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("SELECT dia_semana, tipo_sesion FROM weekly_structure WHERE activo = TRUE")
      var estructura = List[(Int, String)]()
      while (rs.next()) estructura = estructura :+ (rs.getInt("dia_semana"), rs.getString("tipo_sesion"))

      val hoy = LocalDate.now()
      var faltantes = List[Double]()
      for (i <- 0 until days) {
        val fecha = hoy.minusDays(i)
        val diaSemana = fecha.getDayOfWeek.getValue
        estructura.filter(_._1 == diaSemana).foreach { case (_, tipoSesion) =>
          if (!sesionRegistrada(conn, fecha, tipoSesion)) faltantes = faltantes :+ (60.0 * 5.0)
        }
      }
      faltantes
    } finally { conn.close() }
  }

  def getWorkloads(days: Int): Seq[Double] = {
    val conn = getConnection()
    var loads = List[Double]()
    try {
      // BLOQUE B: si hay fb_distancia (Footbar), la carga efectiva pondera por la distancia
      // recorrida en el entreno — un mismo RPE con mas km recorridos supone mas carga objetiva.
      val ps = conn.prepareStatement("""
      (SELECT (minutos * 4) as load FROM matches WHERE status='PLAYED' AND fecha >= CURRENT_DATE - ?)
      UNION ALL
      (SELECT (60 * rpe * (1 + COALESCE(fb_distancia, 0) * 0.05)) as load FROM trainings WHERE fecha >= CURRENT_DATE - ?)
    """)
      ps.setInt(1, days); ps.setInt(2, days)
      val rs = ps.executeQuery()
      while(rs.next()) { loads = loads :+ rs.getDouble("load") }
    } finally { conn.close() }
    // BLOQUE B4: sesiones esperadas de la estructura semanal sin registrar, estimadas con RPE=5
    loads ++ sesionesEsperadasNoRegistradas(days)
  }

  // --- GESTION DE CINTURON DE JUDO ---
  def updateJudoBelt(nuevoCinturon: String): Unit = {
    val conn = getConnection(); try {
      // Nos aseguramos de que la columna existe en la tabla seasons
      val ps = conn.prepareStatement("UPDATE seasons SET judo_belt = ? WHERE id = (SELECT MAX(id) FROM seasons)")
      ps.setString(1, nuevoCinturon); ps.executeUpdate()
    } finally { conn.close() }
  }

  def getWeatherPerformance(): Map[String, (Double, Double)] = {
    val conn = getConnection()
    val stats = scala.collection.mutable.Map[String, (Double, Double)]()
    try {
      val query = """
      SELECT clima, AVG(nota) as media_nota, AVG(goles_contra) as media_gc
      FROM matches
      WHERE status='PLAYED' AND clima IS NOT NULL AND clima <> ''
      GROUP BY clima
    """
      // clima NULL (registro rapido, Telegram, CSV importado) daba una clave null que rompia el dashboard
      val rs = conn.createStatement().executeQuery(query)
      while(rs.next()) {
        stats(rs.getString("clima")) = (rs.getDouble("media_nota"), rs.getDouble("media_gc"))
      }
    } finally { conn.close() }
    stats.toMap
  }

  def getTechnicalAlerts(): List[String] = {
    val reviews = getTechnicalReviews().takeRight(3) // Analizamos las ultimas 3
    if (reviews.size < 2) return Nil

    var alerts = List[String]()

    // Ejemplo: Logica para detectar bajada en blocaje
    val blocajes = reviews.map(_.blocaje)
    if (blocajes.last < blocajes.head) {
      alerts = alerts :+ "⚠️ Tendencia a la baja en BLOCAJE. Se recomienda sesion tecnica analitica."
    }

    // Ejemplo: Logica para detectar valentia baja
    if (reviews.last.valentia < 5) {
      alerts = alerts :+ "🔥 Alerta de VALENTIA: Hector necesita refuerzo en salidas 1v1."
    }

    alerts
  }

  def saveAcademicNote(asig: String, nota: Double, tipo: String, notas: String): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("INSERT INTO academic_performance (asignatura, nota, tipo_evaluacion, comentarios) VALUES (?, ?, ?, ?)")
      ps.setString(1, fixEncoding(asig))
      ps.setDouble(2, nota)
      ps.setString(3, tipo)
      ps.setString(4, fixEncoding(notas))
      ps.executeUpdate()
    } finally { conn.close() }
  }

  // ── FASE 2: FUNCIONES COGNITIVAS Y CARGA ─────────────────────────────────

  def getRPEHistory(days: Int = 60): List[(String, Int, Int, Int)] = {
    // (fecha, rpe, calidad, atencion)
    var l = List[(String, Int, Int, Int)]()
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        s"SELECT fecha::TEXT, rpe, calidad, atencion FROM trainings WHERE fecha >= CURRENT_DATE - $days ORDER BY fecha ASC"
      )
      while (rs.next()) l = l :+ (rs.getString(1), rs.getInt(2), rs.getInt(3), rs.getInt(4))
    } finally { conn.close() }
    l
  }

  def getWeeklyLoad(weeks: Int = 12): List[(String, Int, Int)] = {
    // (semana, carga_total=minutos*rpe, num_sesiones)
    var l = List[(String, Int, Int)]()
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(s"""
        SELECT
          TO_CHAR(DATE_TRUNC('week', fecha), 'YYYY-MM-DD') as semana,
          SUM(rpe * 60) as carga,
          COUNT(*) as sesiones
        FROM trainings
        WHERE fecha >= CURRENT_DATE - ${weeks * 7}
        GROUP BY DATE_TRUNC('week', fecha)
        ORDER BY semana ASC
      """)
      while (rs.next()) l = l :+ (rs.getString(1), rs.getInt(2), rs.getInt(3))
    } finally { conn.close() }
    l
  }

  def getSleepMatchCorrelation(): List[(String, Double, Double, String)] = {
    // Para cada partido: (fecha, horas_sueno_noche_anterior, nota_partido, rival)
    var l = List[(String, Double, Double, String)]()
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("""
        SELECT
          m.fecha::TEXT,
          COALESCE(w.horas_sueno, 0) as horas,
          m.nota,
          m.rival
        FROM matches m
        LEFT JOIN wellness w ON w.fecha = m.fecha - INTERVAL '1 day'
        WHERE m.status = 'PLAYED' AND m.nota > 0
        ORDER BY m.fecha DESC
        LIMIT 30
      """)
      while (rs.next()) l = l :+ (rs.getString(1), rs.getDouble(2), rs.getDouble(3), rs.getString(4))
    } finally { conn.close() }
    l
  }

  def getSleepHistory(days: Int = 60): List[(String, Double, Int, Int)] = {
    // (fecha, horas_sueno, calidad_sueno(1-5), animo)
    var l = List[(String, Double, Int, Int)]()
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        s"SELECT fecha::TEXT, horas_sueno, sueno, animo FROM wellness WHERE fecha >= CURRENT_DATE - $days ORDER BY fecha ASC"
      )
      while (rs.next()) l = l :+ (rs.getString(1), rs.getDouble(2), rs.getInt(3), rs.getInt(4))
    } finally { conn.close() }
    l
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // DIARIO DE CARGA Y SUENO — CORRELACIONES SUENO-RENDIMIENTO (smartwatch)
  // ─────────────────────────────────────────────────────────────────────────────
  case class RegistroPartido(
    fecha: String, suenoProfundoMin: Option[Int], suenoLigeroMin: Option[Int], suenoDespiertoMin: Option[Int],
    horasSueno: Double, calidad: Int, energia: Int, animo: Int, notaPartido: Double,
    acuteLoad: Double, chronicLoad: Double
  ) {
    def acwr: Double = if (chronicLoad > 0) acuteLoad / chronicLoad else 0.0
  }

  private def fetchRegistrosPartido(seasonId: Int = 0): List[RegistroPartido] = {
    val conn = getConnection()
    try {
      val sf = seasonFilter(seasonId).replace("season_id", "m.season_id")
      val rs = conn.createStatement().executeQuery(s"""
        SELECT w.fecha::TEXT as w_fecha,
               w.sueno_profundo_min, w.sueno_ligero_min, w.sueno_despierto_min,
               w.horas_sueno, w.sueno as calidad, w.energia, w.animo,
               m.nota,
               (SELECT COALESCE(SUM(CASE WHEN src = 0 THEN minutos * 4 ELSE 60 * rpe END), 0) / 7.0
                  FROM ((SELECT minutos, 0 as rpe, 0 as src, fecha FROM matches WHERE status = 'PLAYED')
                        UNION ALL
                        (SELECT 0, rpe, 1, fecha FROM trainings)) loads
                  WHERE fecha <= w.fecha AND ${DateUtils.daysBetweenSQL("w.fecha", "fecha")} < 7) as acute_load,
               (SELECT COALESCE(SUM(CASE WHEN src = 0 THEN minutos * 4 ELSE 60 * rpe END), 0) / 28.0
                  FROM ((SELECT minutos, 0 as rpe, 0 as src, fecha FROM matches WHERE status = 'PLAYED')
                        UNION ALL
                        (SELECT 0, rpe, 1, fecha FROM trainings)) loads
                  WHERE fecha <= w.fecha AND ${DateUtils.daysBetweenSQL("w.fecha", "fecha")} < 28) as chronic_load
        FROM wellness w
        JOIN matches m ON m.status = 'PLAYED' AND m.fecha > w.fecha AND m.fecha <= w.fecha + 2 $sf
        ORDER BY w.fecha ASC
      """)
      var list = List[RegistroPartido]()
      while (rs.next()) {
        val spObj = rs.getObject("sueno_profundo_min");  val sp = if (spObj == null) None else Some(rs.getInt("sueno_profundo_min"))
        val slObj = rs.getObject("sueno_ligero_min");     val sl = if (slObj == null) None else Some(rs.getInt("sueno_ligero_min"))
        val sdObj = rs.getObject("sueno_despierto_min");  val sd = if (sdObj == null) None else Some(rs.getInt("sueno_despierto_min"))
        list = list :+ RegistroPartido(
          rs.getString("w_fecha"), sp, sl, sd, rs.getDouble("horas_sueno"),
          rs.getInt("calidad"), rs.getInt("energia"), rs.getInt("animo"), rs.getDouble("nota"),
          rs.getDouble("acute_load"), rs.getDouble("chronic_load")
        )
      }
      list
    } finally { conn.close() }
  }

  private def suenoProfundoNivel(sp: Option[Int]): String = sp match {
    case Some(v) if v > 90 => "ALTO"; case Some(v) if v >= 60 => "MEDIO"; case _ => "BAJO"
  }
  private def horasNivel(h: Double): String = if (h >= 9) "MUCHO" else if (h >= 7) "NORMAL" else "POCO"
  private def escala15Nivel(v: Int): String = if (v >= 4) "ALTO" else if (v == 3) "MEDIO" else "BAJO"
  private def acwrNivel(v: Double): String = if (v > 1.3) "ALTO" else if (v >= 0.8) "NORMAL" else "BAJO"

  private def nivelStats(regs: List[RegistroPartido], niveles: List[String], nivelFn: RegistroPartido => String): List[Map[String, Any]] =
    niveles.map { n =>
      val grp = regs.filter(r => nivelFn(r) == n)
      Map[String, Any]("nivel" -> n, "notaMedia" -> (if (grp.nonEmpty) grp.map(_.notaPartido).sum / grp.size else 0.0), "partidos" -> grp.size)
    }

  // BLOQUE B3: seasonId=0 = historico completo (comportamiento anterior, sin cambios)
  def getSleepCorrelations(seasonId: Int = 0): Map[String, Any] = {
    val all = fetchRegistrosPartido(seasonId)
    val totalPares = all.size

    val conSuenoProfundo = all.filter(_.suenoProfundoMin.isDefined)
    val suenoProfundoList = if (conSuenoProfundo.size >= 3) nivelStats(conSuenoProfundo, List("ALTO", "MEDIO", "BAJO"), r => suenoProfundoNivel(r.suenoProfundoMin)) else List.empty[Map[String, Any]]

    val conHoras = all.filter(_.horasSueno > 0)
    val horasList = if (conHoras.size >= 3) nivelStats(conHoras, List("MUCHO", "NORMAL", "POCO"), r => horasNivel(r.horasSueno)) else List.empty[Map[String, Any]]

    val conCalidad = all.filter(_.calidad > 0)
    val calidadList = if (conCalidad.size >= 3) nivelStats(conCalidad, List("ALTO", "MEDIO", "BAJO"), r => escala15Nivel(r.calidad)) else List.empty[Map[String, Any]]

    val conEnergia = all.filter(_.energia > 0)
    val energiaList = if (conEnergia.size >= 3) nivelStats(conEnergia, List("ALTO", "MEDIO", "BAJO"), r => escala15Nivel(r.energia)) else List.empty[Map[String, Any]]

    val conAnimo = all.filter(_.animo > 0)
    val animoList = if (conAnimo.size >= 3) nivelStats(conAnimo, List("ALTO", "MEDIO", "BAJO"), r => escala15Nivel(r.animo)) else List.empty[Map[String, Any]]

    // Combinacion optima: busca la combinacion horas x sueno profundo x energia con mejor nota media (min 2 pares)
    val combinacionOptima: Option[Map[String, Any]] = {
      val candidatos = all.filter(r => r.horasSueno > 0 && r.energia > 0)
      if (candidatos.size < 2) None
      else {
        val grupos = candidatos.groupBy(r => (horasNivel(r.horasSueno), suenoProfundoNivel(r.suenoProfundoMin), escala15Nivel(r.energia)))
        val validos = grupos.filter(_._2.size >= 2)
        if (validos.isEmpty) None
        else {
          val ((hNivel, spNivel, eNivel), mejores) = validos.maxBy { case (_, grp) => grp.map(_.notaPartido).sum / grp.size }
          val notaMedia = mejores.map(_.notaPartido).sum / mejores.size
          def hLabel(n: String) = n match { case "MUCHO" => "más de 9h"; case "NORMAL" => "entre 7 y 9h"; case _ => "menos de 7h" }
          def spLabel(n: String) = n match { case "ALTO" => "más de 90min"; case "MEDIO" => "entre 60 y 90min"; case _ => "menos de 60min (o sin datos)" }
          def eLabel(n: String) = n match { case "ALTO" => "≥4"; case "MEDIO" => "=3"; case _ => "≤2" }
          val texto = s"Cuando duerme ${hLabel(hNivel)} + sueño profundo ${spLabel(spNivel)} + energía ${eLabel(eNivel)}, su nota media es ${f"$notaMedia%.1f"}"
          Some(Map[String, Any]("texto" -> texto, "notaMedia" -> notaMedia, "partidos" -> mejores.size))
        }
      }
    }

    // ACWR vs sueno: para cada nivel de ACWR, horas medias de sueno del registro
    val conAcwr = all.filter(r => r.chronicLoad > 0 && r.horasSueno > 0)
    val acwrSuenoList = if (conAcwr.size >= 3) {
      List("ALTO", "NORMAL", "BAJO").map { n =>
        val grp = conAcwr.filter(r => acwrNivel(r.acwr) == n)
        Map[String, Any]("nivel" -> n, "horasMedia" -> (if (grp.nonEmpty) grp.map(_.horasSueno).sum / grp.size else 0.0), "partidos" -> grp.size)
      }
    } else List.empty[Map[String, Any]]

    // C2: Nutricion pre-partido vs nota (independiente del cruce con wellness; min 5 partidos con el campo relleno)
    val nutricionList: List[Map[String, Any]] = {
      val connN = getConnection()
      try {
        val rs = connN.createStatement().executeQuery("""
          SELECT nutricion_prepartido, AVG(nota) as nota_media, COUNT(*) as cnt
          FROM matches WHERE status = 'PLAYED' AND nutricion_prepartido IS NOT NULL AND nutricion_prepartido <> ''
          GROUP BY nutricion_prepartido
        """)
        var grupos = List[(String, Double, Int)]()
        while (rs.next()) grupos = grupos :+ (rs.getString("nutricion_prepartido"), rs.getDouble("nota_media"), rs.getInt("cnt"))
        val total = grupos.map(_._3).sum
        if (total < 5) List.empty[Map[String, Any]]
        else grupos.sortBy(-_._2).map { case (tipo, notaMedia, cnt) =>
          Map[String, Any]("tipo" -> tipo, "notaMedia" -> notaMedia, "partidos" -> cnt)
        }
      } finally { connN.close() }
    }

    Map(
      "totalPares"        -> totalPares,
      "suenoProfundo"     -> suenoProfundoList,
      "horasTotales"      -> horasList,
      "calidad"           -> calidadList,
      "energia"           -> energiaList,
      "animo"             -> animoList,
      "combinacionOptima" -> combinacionOptima,
      "acwrSueno"         -> acwrSuenoList,
      "nutricion"         -> nutricionList
    )
  }

  // Lectura desde cache unicamente — nunca llama a Gemini en el render de pagina
  def getSleepAnalysisCached(): Option[String] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT payload FROM feature_cache WHERE cache_key = 'sleep_correlations_ia' AND updated_at > NOW() - INTERVAL '7 days'"
      )
      if (rs.next()) Some(ujson.read(rs.getString("payload"))("analisis").str) else None
    } finally { conn.close() }
  }

  // Llamada real a Gemini — SOLO se invoca desde el boton POST explicito "Analisis IA completo"
  def generateSleepAnalysisIA(): String = {
    val conn = getConnection()
    try {
      val d = getSleepCorrelations()
      val card = getLatestCardData()
      val edad = calcularEdadExacta(card.fechaNacimiento)

      def fmtList(nombre: String, l: List[Map[String, Any]]): String =
        if (l.isEmpty) s"$nombre: sin datos suficientes"
        else s"$nombre: " + l.map(x => s"${x("nivel")}=${f"${x("notaMedia").asInstanceOf[Double]}%.1f"}(${x("partidos")}p)").mkString(", ")

      val suenoProfundo     = d("suenoProfundo").asInstanceOf[List[Map[String, Any]]]
      val horasTotales      = d("horasTotales").asInstanceOf[List[Map[String, Any]]]
      val calidad           = d("calidad").asInstanceOf[List[Map[String, Any]]]
      val energia            = d("energia").asInstanceOf[List[Map[String, Any]]]
      val animo               = d("animo").asInstanceOf[List[Map[String, Any]]]
      val combinacionOptima   = d("combinacionOptima").asInstanceOf[Option[Map[String, Any]]]
      val acwrSueno            = d("acwrSueno").asInstanceOf[List[Map[String, Any]]]
      val nutricion             = d("nutricion").asInstanceOf[List[Map[String, Any]]]

      val datosStr = List(
        fmtList("Sueño profundo", suenoProfundo), fmtList("Horas totales", horasTotales),
        fmtList("Calidad subjetiva", calidad), fmtList("Energía", energia), fmtList("Ánimo", animo),
        combinacionOptima.map(c => s"Combinación óptima: ${c("texto")}").getOrElse("Combinación óptima: sin datos suficientes"),
        (if (acwrSueno.nonEmpty) "ACWR vs horas de sueño: " + acwrSueno.map(x => s"${x("nivel")}=${f"${x("horasMedia").asInstanceOf[Double]}%.1f"}h(${x("partidos")}p)").mkString(", ") else "ACWR vs sueño: sin datos suficientes"),
        (if (nutricion.nonEmpty) "Nutrición prepartido: " + nutricion.map(x => s"${x("tipo")}=${f"${x("notaMedia").asInstanceOf[Double]}%.1f"}(${x("partidos")}p)").mkString(", ") else "Nutrición prepartido: sin datos suficientes")
      ).mkString(". ")

      val prompt = s"""Eres el analista de rendimiento de Héctor, portero de $edad años. Estos son sus datos de correlación entre sueño y rendimiento en partido: $datosStr. Analiza: 1) Cuál es el factor de sueño que más impacta en su rendimiento, 2) Qué pauta de sueño debería seguir la noche antes de un partido, 3) Si hay alguna señal de que la carga de entrenamiento está afectando la calidad del sueño. Responde en texto plano, máximo 3 líneas por punto. Adapta el lenguaje para que el padre pueda entenderlo y actuar."""

      val analisis = AIProvider.ask(prompt, None, bypassCache = true)

      val payload = ujson.Obj("analisis" -> analisis)
      val upsert = conn.prepareStatement("""
        INSERT INTO feature_cache (cache_key, payload, updated_at) VALUES ('sleep_correlations_ia', ?, NOW())
        ON CONFLICT (cache_key) DO UPDATE SET payload = EXCLUDED.payload, updated_at = NOW()
      """)
      upsert.setString(1, ujson.write(payload))
      upsert.executeUpdate()

      analisis
    } finally { conn.close() }
  }

  // Insight deterministico (sin Gemini) para el dashboard — se puede llamar en el render
  case class SleepFactorDef(mejorLabel: String, key: String, nivelMejor: String, nivelPeor: String)
  private val sleepDashboardFactores = List(
    SleepFactorDef("más de 90min de sueño profundo", "suenoProfundo", "ALTO", "BAJO"),
    SleepFactorDef("más de 9h de sueño", "horasTotales", "MUCHO", "POCO"),
    SleepFactorDef("una calidad de sueño alta (4-5)", "calidad", "ALTO", "BAJO"),
    SleepFactorDef("un nivel alto de energía", "energia", "ALTO", "BAJO"),
    SleepFactorDef("un buen estado de ánimo", "animo", "ALTO", "BAJO")
  )

  def getSleepDashboardInsight(): Option[String] = {
    val d = getSleepCorrelations()
    val totalPares = d("totalPares").asInstanceOf[Int]
    if (totalPares < 5) return None

    val diffs = sleepDashboardFactores.flatMap { f =>
      val niveles = d(f.key).asInstanceOf[List[Map[String, Any]]]
      val mejor = niveles.find(n => n("nivel") == f.nivelMejor && n("partidos").asInstanceOf[Int] > 0)
      val peor  = niveles.find(n => n("nivel") == f.nivelPeor  && n("partidos").asInstanceOf[Int] > 0)
      (mejor, peor) match {
        case (Some(m), Some(p)) =>
          val diff = m("notaMedia").asInstanceOf[Double] - p("notaMedia").asInstanceOf[Double]
          Some((f, diff))
        case _ => None
      }
    }

    if (diffs.isEmpty) None
    else {
      val (mejorFactor, diff) = diffs.maxBy(_._2)
      if (diff <= 0) None else Some(f"💤 Cuando duerme ${mejorFactor.mejorLabel}, su nota sube $diff%.1f puntos de media.")
    }
  }

  def getFatigaDetector(days: Int = 30): List[(String, Int, Double, Int)] = {
    // (fecha, atencion_entreno, nota_academica, animo_wellness)
    var l = List[(String, Int, Double, Int)]()
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(s"""
        SELECT
          t.fecha::TEXT,
          t.atencion,
          COALESCE(a.nota, 0) as nota_acad,
          COALESCE(w.animo, 3) as animo
        FROM trainings t
        LEFT JOIN academic_performance a ON a.fecha = t.fecha
        LEFT JOIN wellness w ON w.fecha = t.fecha
        WHERE t.fecha >= CURRENT_DATE - $days
        ORDER BY t.fecha ASC
      """)
      while (rs.next()) l = l :+ (rs.getString(1), rs.getInt(2), rs.getDouble(3), rs.getInt(4))
    } finally { conn.close() }
    l
  }

  def getNotificationAlerts(): List[(String, String, String)] = {
    val alerts = scala.collection.mutable.ListBuffer[(String, String, String)]()
    val conn = getConnection()
    try {
      // 1. ACWR riesgo — calculado directo sin sub-conexion
      val rsAcute = conn.prepareStatement(
        "SELECT COALESCE(SUM(rpe * 60), 0) FROM trainings WHERE fecha >= CURRENT_DATE - ?"
      )
      rsAcute.setInt(1, 7); val rsA = rsAcute.executeQuery()
      val acuteLoad = if (rsA.next()) rsA.getDouble(1) else 0.0

      val rsChronic = conn.prepareStatement(
        "SELECT COALESCE(SUM(rpe * 60), 0) FROM trainings WHERE fecha >= CURRENT_DATE - ?"
      )
      rsChronic.setInt(1, 28); val rsC = rsChronic.executeQuery()
      val chronicLoad = if (rsC.next()) rsC.getDouble(1) else 0.0

      val acwr = if (chronicLoad > 0) (acuteLoad / 7.0) / (chronicLoad / 28.0) else 0.0
      if (acwr > umbralesACWR().riesgo) alerts += (("danger", "ACWR ALTO", s"Ratio carga: ${f"$acwr%.2f"} — Riesgo de lesion"))

      // 2. Sin registrar wellness hoy
      val rsW = conn.createStatement().executeQuery("SELECT COUNT(*) FROM wellness WHERE fecha = CURRENT_DATE")
      if (rsW.next() && rsW.getInt(1) == 0)
        alerts += (("warning", "SIN WELLNESS", "No has registrado tu estado fisico hoy"))

      // 3. Proximo partido en menos de 3 dias
      val rsM = conn.createStatement().executeQuery(
        "SELECT rival, fecha FROM matches WHERE status='SCHEDULED' ORDER BY fecha ASC LIMIT 1"
      )
      if (rsM.next()) {
        val fechaPartido = rsM.getDate("fecha").toLocalDate
        val diasRestantes = java.time.temporal.ChronoUnit.DAYS.between(java.time.LocalDate.now(), fechaPartido)
        if (diasRestantes <= 3 && diasRestantes >= 0)
          alerts += (("info", s"PARTIDO EN ${diasRestantes}D", s"vs ${fixEncoding(rsM.getString("rival"))} — ${rsM.getDate("fecha")}"))
      }

      // 4. Baja atencion sostenida
      val rsAtt = conn.createStatement().executeQuery(
        "SELECT COUNT(*) FROM trainings WHERE fecha >= CURRENT_DATE - 7 AND atencion < 6"
      )
      if (rsAtt.next() && rsAtt.getInt(1) >= 3)
        alerts += (("warning", "FATIGA MENTAL", "3+ sesiones con baja atencion esta semana"))

    } catch { case _: Exception => } finally { conn.close() }
    alerts.toList
  }

   def getCognitiveInsight(): String = {
    val conn = getConnection()
    try {
      // Buscamos la media de notas de los ultimos 30 dias
      val rsAcad = conn.createStatement().executeQuery("SELECT AVG(nota) FROM academic_performance WHERE fecha > CURRENT_DATE - 30")
      val avgAcad = if(rsAcad.next()) rsAcad.getDouble(1) else 0.0

      // Buscamos la media de 'atencion' en los entrenamientos de los ultimos 30 dias
      val rsTrain = conn.createStatement().executeQuery("SELECT AVG(atencion) FROM trainings WHERE fecha > CURRENT_DATE - 30")
      val avgAtt = if(rsTrain.next()) rsTrain.getDouble(1) else 0.0

      val baseMsg = if (avgAcad > 0 && avgAtt > 0) {
        if (avgAcad < 6.0 && avgAtt < 7.0)
          "🧠 **ALERTA COGNITIVA**: Baja concentracion detectada en ambos entornos. Posible fatiga mental general."
        else if (avgAcad > 8.0 && avgAtt < 6.0)
          "⚽ **DESCONEXION**: Alto rendimiento academico pero baja atencion en campo. ¿Falta de motivacion deportiva?"
        else
          "✅ **SINERGIA OPTIMA**: Equilibrio detectado entre estudios y deporte."
      } else "Faltan datos para analisis cognitivo."

      // Footbar: contexto de carga fisica de los ultimos 5 partidos (senal de fatiga)
      val rsFb = conn.createStatement().executeQuery("""
        SELECT AVG(f.distancia_km) AS dist_media, AVG(f.sprint_max_kmh) AS sprint_media,
               AVG(m.nota) AS nota_media, COUNT(*) AS n
        FROM (SELECT id, nota FROM matches WHERE status='PLAYED' ORDER BY fecha DESC LIMIT 5) m
        JOIN footbar_sessions f ON f.match_id = m.id
      """)
      val fatigaMsg =
        if (rsFb.next() && rsFb.getInt("n") >= 2) {
          val distMedia = rsFb.getDouble("dist_media")
          val sprintMedia = rsFb.getDouble("sprint_media")
          val notaMediaFb = rsFb.getDouble("nota_media")
          if (notaMediaFb < 6.0 && (distMedia > 3.0 || sprintMedia > 24.0))
            " [Footbar] **CARGA FISICA ALTA**: Los ultimos partidos con datos Footbar muestran distancia/sprint elevados junto a notas bajas — posible senal de fatiga."
          else ""
        } else ""

      baseMsg + fatigaMsg
    } finally { conn.close() }
  }

  def saveMedicalReport(fecha: String, tipo: String, fileBase64: String, esPrevio: Boolean): String = {
    val conn = getConnection()
    try {
      val prompt = """
      Analiza este informe medico de un nino deportista.
      Extrae: 1) Diagnostico claro. 2) Impacto en el deporte (ej: limitar saltos, reposo).
      3) Si es una analitica, destaca valores fuera de rango.
      Responde en formato: DIAGNOSTICO: ... | RECOMENDACION: ...
    """

      // Detectamos el formato para el AIProvider
      val mime = if (fileBase64.contains("pdf")) "application/pdf" else "image/jpeg"

      // La magia: AIProvider.ask devolvera el analisis de la cache si ya se subio este mismo archivo
      val analisisIA = AIProvider.ask(prompt, Some((mime, fileBase64)))

      val partes = analisisIA.split("\\|")
      val diag = partes.headOption.getOrElse("No detectado").replace("DIAGNOSTICO:", "").trim
      val rec = partes.lastOption.getOrElse("No detectado").replace("RECOMENDACION:", "").trim

      val ps = conn.prepareStatement("INSERT INTO medical_vault (fecha_informe, tipo_informe, diagnostico_ia, recomendaciones_ia, es_previo_futbol) VALUES (?, ?, ?, ?, ?)")
      ps.setDate(1, java.sql.Date.valueOf(fecha))
      ps.setString(2, tipo)
      ps.setString(3, fixEncoding(diag))
      ps.setString(4, fixEncoding(rec))
      ps.setBoolean(5, esPrevio)
      ps.executeUpdate()

      s"Informe procesado: $diag"
    } catch { case e: Exception => s"Error medico: ${e.getMessage}" } finally { conn.close() }
  }

  def getLatestMedicalInsight(): String = {
    val conn = getConnection()
    try {
      // Buscamos el ultimo informe que no sea un simple 'Baseline' previo
      val query = """
      SELECT diagnostico_ia, recomendaciones_ia
      FROM medical_vault
      WHERE es_previo_futbol = FALSE
      ORDER BY fecha_informe DESC LIMIT 1
    """
      val rs = conn.createStatement().executeQuery(query)
      if (rs.next()) {
        val diag = rs.getString("diagnostico_ia")
        val rec = rs.getString("recomendaciones_ia")
        // Retornamos un string combinado para el widget
        s"$diag. RECOMENDACION: $rec"
      } else ""
    } catch {
      case _: Exception => ""
    } finally {
      conn.close()
    }
  }

  def saveMedicalRecordFull(fecha: String, tipo: String, diag: String, rec: String, esPrevio: Boolean): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement(
        "INSERT INTO medical_vault (fecha_informe, tipo_informe, diagnostico_ia, recomendaciones_ia, es_previo_futbol) VALUES (?, ?, ?, ?, ?)"
      )
      ps.setDate(1, java.sql.Date.valueOf(fecha))
      ps.setString(2, tipo)
      ps.setString(3, fixEncoding(diag))
      ps.setString(4, fixEncoding(rec))
      ps.setBoolean(5, esPrevio)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE B — CONTRACT & LICENSE VAULT (Elite exclusivamente)
  // ─────────────────────────────────────────────────────────────────────────────
  def saveDocumentVault(tipo: String, nombre: String, fecha: String, archivoB64: String, notas: String): Int = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement(
        "INSERT INTO document_vault (tipo, nombre, fecha, archivo_b64, notas) VALUES (?, ?, ?::date, ?, ?) RETURNING id")
      ps.setString(1, tipo)
      ps.setString(2, fixEncoding(nombre))
      ps.setString(3, fecha)
      ps.setString(4, archivoB64)
      ps.setString(5, fixEncoding(notas))
      val rs = ps.executeQuery()
      if (rs.next()) rs.getInt("id") else 0
    } finally { conn.close() }
  }

  def getDocumentVaultList(): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT id, tipo, nombre, fecha, notas FROM document_vault ORDER BY fecha DESC, id DESC")
      var l = List[Map[String, Any]]()
      while (rs.next()) l = l :+ Map(
        "id" -> rs.getInt("id"), "tipo" -> rs.getString("tipo"),
        "nombre" -> fixEncoding(rs.getString("nombre")), "fecha" -> rs.getDate("fecha").toString,
        "notas" -> fixEncoding(Option(rs.getString("notas")).getOrElse(""))
      )
      l
    } finally { conn.close() }
  }

  def getDocumentVaultFile(id: Int): Option[(String, String)] = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("SELECT nombre, archivo_b64 FROM document_vault WHERE id = ?")
      ps.setInt(1, id)
      val rs = ps.executeQuery()
      if (rs.next()) Some((fixEncoding(rs.getString("nombre")), rs.getString("archivo_b64"))) else None
    } finally { conn.close() }
  }

  def getMedicalReports(): List[MedicalReport] = {
    val conn = getConnection()
    val reports = scala.collection.mutable.ListBuffer[MedicalReport]()
    try {
      // Consultamos los informes ordenados por fecha, los mas recientes primero
      val query = """
      SELECT id, fecha_informe, tipo_informe, diagnostico_ia, recomendaciones_ia, es_previo_futbol
      FROM medical_vault
      ORDER BY fecha_informe DESC
    """
      val rs = conn.createStatement().executeQuery(query)
      while (rs.next()) {
        reports += MedicalReport(
          id = rs.getInt("id"),
          fecha = rs.getString("fecha_informe"),
          tipo = rs.getString("tipo_informe"),
          diagnostico = rs.getString("diagnostico_ia"),
          recomendaciones = rs.getString("recomendaciones_ia"),
          esPrevio = rs.getBoolean("es_previo_futbol")
        )
      }
    } catch {
      case e: Exception => println(s"Error recuperando informes medicos: ${e.getMessage}")
    } finally {
      conn.close()
    }
    reports.toList
  }
  def getCoachAdvice(): String = {
    val card = getLatestCardData()
    val matches = getMatchesList().take(3) // Ultimos 3 partidos
    val cog = getCognitiveInsight() // Datos de estudios/atencion
    val wellness = getOracleInsights() // Datos fisicos/crecimiento

    val prompt = s"""
    Actua como un Coach de Elite para un portero de ${calcularEdadExacta(card.fechaNacimiento)} anos.
    CONTEXTO TECNICO: Media ${card.media}, Reflejos ${card.ref}.
    ESTADO FISICO: $wellness
    ESTADO COGNITIVO: $cog
    ULTIMOS PARTIDOS: ${matches.map(m => m.rival + " nota:" + m.nota).mkString(", ")}

    Dame 3 consejos breves y motivadores. Si detectas fatiga o baja atencion academica, prioriza el descanso psicologico.
  """

    // Usamos el AIProvider centralizado con cache (se refresca una vez al dia o tras cambios)
    AIProvider.ask(prompt)
  }
  def analyzeAudioLog(matchId: Int, audioBase64: String): String = {
    val prompt = """Eres el psicólogo deportivo de Héctor, portero de élite. Analiza este audio post-partido con el Protocolo de 4 Anclas. Sé directo y conciso.

ANCLA 1 — ESTADO BIO-EMOCIONAL:
Detecta nivel de energía, fatiga o frustración por tono de voz y mensaje.
Formato: [Motivado/Neutro/Bajón/Frustrado/Eufórico] + 1 frase explicativa

ANCLA 2 — HITO CRÍTICO:
La acción técnica más relevante mencionada (parada clave, fallo en salida, etc.)
Formato: ACIERTO o ERROR: descripción breve

ANCLA 3 — FACTOR EXTERNO:
Menciona clima, campo, árbitro u otros factores externos no numéricos.
Formato: [detectado/no mencionado] + detalle si existe

ANCLA 4 — ENFOQUE DE MEJORA:
Qué aspecto específico quiere trabajar para el próximo partido.
Formato: OBJETIVO: descripción + 1 consejo técnico concreto

Responde en texto plano. Si el audio no cubre un ancla, escribe "No mencionado"."""

    // Llamada al motor unificado con soporte para audio
    val res = AIProvider.ask(prompt, Some(("audio/webm", audioBase64)))

    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("UPDATE matches SET analisis_voz = ? WHERE id = ?")
      ps.setString(1, fixEncoding(res))
      ps.setInt(2, matchId)
      ps.executeUpdate()
    } finally { conn.close() }
    res
  }
  def testAIConnection(): String = {
    try {
      AIProvider.ask("Responde solo con la palabra OK si me escuchas.", None, bypassCache = true)
    } catch {
      case e: Exception => s"Fallo total: ${e.getMessage}"
    }
  }

  // ── MATCH CONTEXT ANALYTICS ─────────────────────────────────────────────
  // BLOQUE B3: seasonId=0 = historico completo (comportamiento anterior, sin cambios)
  def getMatchContextData(seasonId: Int = 0): Map[String, Any] = {
    val conn = getConnection()
    try {
      val sf = seasonFilter(seasonId)
      // 1. POR TIPO DE PARTIDO (LIGA / TORNEO / AMISTOSO)
      val rsTipo = conn.createStatement().executeQuery(s"""
        SELECT
          COALESCE(tipo_partido, 'LIGA') as tipo,
          COUNT(*) as pj,
          AVG(nota) as nota_media,
          AVG(goles_contra) as gc_media,
          SUM(CASE WHEN goles_contra = 0 THEN 1 ELSE 0 END) as limpias,
          AVG(paradas) as paradas_media
        FROM matches WHERE status='PLAYED' $sf
        GROUP BY COALESCE(tipo_partido, 'LIGA')
        ORDER BY nota_media DESC
      """)
      var porTipo = List[Map[String, Any]]()
      while (rsTipo.next()) {
        porTipo = porTipo :+ Map(
          "tipo"    -> rsTipo.getString("tipo"),
          "pj"      -> rsTipo.getInt("pj"),
          "nota"    -> rsTipo.getDouble("nota_media"),
          "gc"      -> rsTipo.getDouble("gc_media"),
          "limpias" -> rsTipo.getInt("limpias"),
          "paradas" -> rsTipo.getDouble("paradas_media")
        )
      }

      // 2. POR CLIMA
      val rsClima = conn.createStatement().executeQuery(s"""
        SELECT
          COALESCE(clima, 'Sin datos') as clima,
          COUNT(*) as pj,
          AVG(nota) as nota_media,
          AVG(goles_contra) as gc_media,
          SUM(CASE WHEN goles_contra = 0 THEN 1 ELSE 0 END) as limpias
        FROM matches WHERE status='PLAYED' AND clima IS NOT NULL AND clima != '' $sf
        GROUP BY COALESCE(clima, 'Sin datos')
        ORDER BY nota_media DESC
      """)
      var porClima = List[Map[String, Any]]()
      while (rsClima.next()) {
        porClima = porClima :+ Map(
          "clima"   -> rsClima.getString("clima"),
          "pj"      -> rsClima.getInt("pj"),
          "nota"    -> rsClima.getDouble("nota_media"),
          "gc"      -> rsClima.getDouble("gc_media"),
          "limpias" -> rsClima.getInt("limpias")
        )
      }

      // 3. LOCAL vs VISITANTE — campo es_local de la tabla matches
      val rsLV = conn.createStatement().executeQuery(s"""
        SELECT
          es_local,
          COUNT(*) as pj,
          AVG(nota) as nota_media,
          AVG(goles_contra) as gc_media,
          SUM(CASE WHEN goles_contra = 0 THEN 1 ELSE 0 END) as limpias
        FROM matches
        WHERE status='PLAYED' AND es_local IS NOT NULL $sf
        GROUP BY es_local
      """)
      var localNota = 0.0; var localGC = 0.0; var localPJ = 0; var localLimpias = 0
      var visitNota = 0.0; var visitGC  = 0.0; var visitPJ = 0; var visitLimpias = 0
      while (rsLV.next()) {
        val esLocal = rsLV.getBoolean("es_local")
        val nota = rsLV.getDouble("nota_media")
        val gc   = rsLV.getDouble("gc_media")
        val pj   = rsLV.getInt("pj")
        val cs   = rsLV.getInt("limpias")
        if (esLocal) {
          localNota = nota; localGC = gc; localPJ = pj; localLimpias = cs
        } else {
          visitNota = nota; visitGC = gc; visitPJ = pj; visitLimpias = cs
        }
      }
      val localNotaFinal = localNota
      val visitNotaFinal = visitNota
      val localGCFinal   = localGC
      val visitGCFinal   = visitGC

      // 4. POR DURACIÓN (franjas de minutos)
      val rsDur = conn.createStatement().executeQuery(s"""
        SELECT
          CASE
            WHEN minutos < 40 THEN 'Partido corto (<40 min)'
            WHEN minutos < 60 THEN 'Media parte (40-59 min)'
            WHEN minutos < 80 THEN 'Normal (60-79 min)'
            ELSE 'Partido completo (80+ min)'
          END as franja,
          COUNT(*) as pj,
          AVG(nota) as nota_media,
          AVG(goles_contra) as gc_media,
          SUM(CASE WHEN goles_contra = 0 THEN 1 ELSE 0 END) as limpias
        FROM matches WHERE status='PLAYED' AND minutos > 0 $sf
        GROUP BY 1
        ORDER BY nota_media DESC
      """)
      var porDuracion = List[Map[String, Any]]()
      while (rsDur.next()) {
        porDuracion = porDuracion :+ Map(
          "franja"  -> rsDur.getString("franja"),
          "pj"      -> rsDur.getInt("pj"),
          "nota"    -> rsDur.getDouble("nota_media"),
          "gc"      -> rsDur.getDouble("gc_media"),
          "limpias" -> rsDur.getInt("limpias")
        )
      }

      // 5. TENDENCIA MENSUAL (últimos 12 meses, para el gráfico de línea)
      val rsTrend = conn.createStatement().executeQuery(s"""
        SELECT
          TO_CHAR(DATE_TRUNC('month', fecha), 'MM/YY') as mes,
          AVG(nota) as nota_media,
          COUNT(*) as pj,
          SUM(CASE WHEN goles_contra = 0 THEN 1 ELSE 0 END) as limpias
        FROM matches
        WHERE status='PLAYED' AND fecha >= CURRENT_DATE - INTERVAL '12 months' $sf
        GROUP BY DATE_TRUNC('month', fecha)
        ORDER BY DATE_TRUNC('month', fecha) ASC
      """)
      var trendLabels = List[String]()
      var trendNotas  = List[Double]()
      var trendPJs    = List[Int]()
      while (rsTrend.next()) {
        trendLabels = trendLabels :+ rsTrend.getString("mes")
        trendNotas  = trendNotas  :+ rsTrend.getDouble("nota_media")
        trendPJs    = trendPJs    :+ rsTrend.getInt("pj")
      }

      // 6. MEJOR Y PEOR CONTEXTO (resumen ejecutivo)
      val allContexts: List[(String, Double, Int)] = (
        porTipo.map(m => (m("tipo").toString, m("nota").asInstanceOf[Double], m("pj").asInstanceOf[Int])) ++
        porClima.map(m => (m("clima").toString, m("nota").asInstanceOf[Double], m("pj").asInstanceOf[Int]))
      ).filter(_._3 >= 2) // mínimo 2 partidos para ser significativo

      val mejorCtx = allContexts.sortBy(-_._2).headOption
      val peorCtx  = allContexts.sortBy(_._2).headOption

      // 7. Totales globales para referencia
      val rsGlobal = conn.createStatement().executeQuery(
        "SELECT COUNT(*) as pj, AVG(nota) as nota, AVG(goles_contra) as gc FROM matches WHERE status='PLAYED'"
      )
      val (totalPJ, notaGlobal, gcGlobal) = if (rsGlobal.next())
        (rsGlobal.getInt("pj"), rsGlobal.getDouble("nota"), rsGlobal.getDouble("gc"))
      else (0, 0.0, 0.0)

      Map(
        "porTipo"        -> porTipo,
        "porClima"       -> porClima,
        "porDuracion"    -> porDuracion,
        "localNota"      -> localNotaFinal,
        "localGC"        -> localGCFinal,
        "localPJ"        -> localPJ,
        "localLimpias"   -> localLimpias,
        "visitNota"      -> visitNotaFinal,
        "visitGC"        -> visitGCFinal,
        "visitPJ"        -> visitPJ,
        "visitLimpias"   -> visitLimpias,
        "trendLabels"    -> trendLabels,
        "trendNotas"     -> trendNotas,
        "trendPJs"       -> trendPJs,
        "mejorCtx"       -> mejorCtx,
        "peorCtx"        -> peorCtx,
        "totalPJ"        -> totalPJ,
        "notaGlobal"     -> notaGlobal,
        "gcGlobal"       -> gcGlobal
      )
    } finally { conn.close() }
  }

  // ── BYPASS RATE POR TEMPORADA (Fase 6.5 completion) ─────────────────────
  def getBypassRateEvolution(): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("""
        SELECT
          EXTRACT(YEAR FROM fecha)::TEXT as anio,
          AVG(lineas_superadas) as bp_media,
          AVG(CASE WHEN acciones_pie > 0 THEN lineas_superadas::FLOAT / acciones_pie ELSE NULL END) as bp_efic,
          COUNT(*) as pj,
          SUM(lineas_superadas) as bp_total
        FROM matches
        WHERE status='PLAYED' AND lineas_superadas > 0
        GROUP BY EXTRACT(YEAR FROM fecha)
        ORDER BY anio ASC
      """)
      var rows = List[Map[String, Any]]()
      while (rs.next()) {
        rows = rows :+ Map(
          "anio"     -> rs.getString("anio"),
          "bpMedia"  -> rs.getDouble("bp_media"),
          "bpEfic"   -> rs.getDouble("bp_efic"),
          "pj"       -> rs.getInt("pj"),
          "bpTotal"  -> rs.getInt("bp_total")
        )
      }
      rows
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // FASE 7 v7.2 — MARKET ESTIMATOR
  // Regresion lineal multivariable sobre metricas existentes.
  // Variables: nota_media, psxg_delta, spv_score, bypass_efic, bio_factor, edad
  // Output: valor de mercado formativo estimado (€) + percentil vs academias
  // ─────────────────────────────────────────────────────────────────────────────
  def getMarketEstimatorData(): Map[String, Any] = {
    val conn = getConnection()
    try {
      // ── 1. Metricas base ────────────────────────────────────────────────────
      val rsBase = conn.createStatement().executeQuery("""
        SELECT
          COALESCE(AVG(nota), 0.0)                                             AS nota_media,
          COALESCE(AVG(paradas), 0.0)                                          AS par_media,
          COALESCE(AVG(paradas_1v1), 0.0)                                      AS par1v1_media,
          COALESCE(AVG(paradas_aereas), 0.0)                                   AS paer_media,
          COALESCE(AVG(CASE WHEN acciones_pie > 0
                    THEN lineas_superadas::FLOAT / acciones_pie END), 0.0)     AS bypass_efic,
          COALESCE(AVG(lineas_superadas), 0.0)                                 AS bypass_vol,
          COUNT(*) FILTER (WHERE goles_contra = 0)                            AS limpias,
          COUNT(*)                                                             AS pj,
          COALESCE(
            SUM(CASE WHEN goles_favor > goles_contra THEN 1 ELSE 0 END)::FLOAT
            / NULLIF(COUNT(*), 0), 0.0)                                       AS win_rate
        FROM matches WHERE status='PLAYED'
      """)

      var notaMedia = 0.0; var par1v1 = 0.0; var parAer = 0.0
      var bypassEfic = 0.0; var bypassVol = 0.0; var limpias = 0
      var pj = 0; var winRate = 0.0; var parMedia = 0.0
      if (rsBase.next()) {
        notaMedia  = rsBase.getDouble("nota_media")
        parMedia   = rsBase.getDouble("par_media")
        par1v1     = rsBase.getDouble("par1v1_media")
        parAer     = rsBase.getDouble("paer_media")
        bypassEfic = rsBase.getDouble("bypass_efic")
        bypassVol  = rsBase.getDouble("bypass_vol")
        limpias    = rsBase.getInt("limpias")
        pj         = rsBase.getInt("pj")
        winRate    = rsBase.getDouble("win_rate")
      }

      // ── 2. PSxG delta (goles esperados - reales) ────────────────────────────
      val rsPsxg = conn.createStatement().executeQuery("""
        SELECT
          COALESCE(AVG(goles_contra), 0) as gc_media,
          COALESCE(AVG(
            CASE zona_goles
              WHEN '5' THEN 0.85 WHEN '4' THEN 0.65 WHEN '6' THEN 0.65
              WHEN '2' THEN 0.45 WHEN '8' THEN 0.45
              ELSE 0.30
            END
          ), 0) as xg_media
        FROM matches
        WHERE status='PLAYED' AND goles_contra > 0
      """)
      var psxgDelta = 0.0
      if (rsPsxg.next()) {
        val gcReal = rsPsxg.getDouble("gc_media")
        val xgEsperado = rsPsxg.getDouble("xg_media")
        psxgDelta = xgEsperado - gcReal  // positivo = mejor que esperado
      }

      // ── 3. Edad y bio-banding ───────────────────────────────────────────────
      val rsEdad = conn.createStatement().executeQuery(
        "SELECT fecha_nacimiento FROM seasons ORDER BY id DESC LIMIT 1")
      val fechaNac = if (rsEdad.next())
        Option(rsEdad.getDate("fecha_nacimiento")).map(_.toString).getOrElse("2015-06-19")
      else "2015-06-19"
      val hoy    = java.time.LocalDate.now()
      val nac    = java.time.LocalDate.parse(fechaNac)
      val edad   = java.time.Period.between(nac, hoy).getYears
      // Factor bio-banding: mayor = más avanzado en madurez (ventaja para clubes)
      val bioFactor: Double = if (edad <= 10) 0.70 else if (edad <= 12) 0.80
                               else if (edad <= 14) 0.90 else if (edad <= 16) 1.00
                               else 1.10

      // ── 4. SPV calculado inline ─────────────────────────────────────────────
      val spvEfic: Double = {
        val total = par1v1 * 1.5 + parAer * 1.2 + (parMedia - par1v1 - parAer)
        if (total > 0) math.min(100.0, total * 10.0) else 0.0
      }

      // ── 5. REGRESION LINEAL MULTIVARIABLE ───────────────────────────────────
      // Formula derivada de la literatura de scouting formativo (pesos calibrados):
      //   V = nota_normalizada * 35
      //     + spv_normalizado   * 20
      //     + bypass_efic       * 15
      //     + psxg_delta_norm   * 15
      //     + win_rate          * 10
      //     + bio_factor_boost  *  5
      // Resultado en unidades de "puntuación de valor" (0-100) → mapeado a €
      // notaMedia esta en escala 0-10 (campo "nota" del Match Center) — se escala a
      // 0-100 antes de normalizar contra el rango de referencia 40-100.
      val notaNorm:   Double = math.max(0, math.min(1.0, (notaMedia * 10.0 - 40.0) / 60.0))
      val spvNorm:    Double = math.max(0, math.min(1.0, spvEfic / 100.0))
      val bypassNorm: Double = math.max(0, math.min(1.0, bypassEfic))
      val psxgNorm:   Double = math.max(0, math.min(1.0, (psxgDelta + 2.0) / 4.0))
      val bioBoost:   Double = (bioFactor - 0.7) / 0.4  // 0-1 range

      val rawScore: Double =
        notaNorm   * 35.0 +
        spvNorm    * 20.0 +
        bypassNorm * 15.0 +
        psxgNorm   * 15.0 +
        winRate    * 10.0 +
        bioBoost   *  5.0

      // Escala de valor formativo: 0-100 pts → 0 a 150.000 €
      // (referencia: porteros de academia sub-16 elite: 30k-80k; sub-14: 10k-40k)
      val valorEstimado: Int = (rawScore * 1500).toInt

      // ── 6. Percentil vs academias españolas (tabla de referencia calibrada) ──
      // Percentiles empiricos por edad para porteros de academia regional/nacional
      val percentilesRef: Map[Int, List[Int]] = Map(
        // edad -> [p10, p25, p50, p75, p90] en puntos rawScore
        9  -> List(15, 22, 35, 48, 62),
        10 -> List(18, 26, 38, 51, 65),
        11 -> List(20, 29, 42, 55, 68),
        12 -> List(22, 32, 45, 58, 71),
        13 -> List(25, 35, 48, 62, 74),
        14 -> List(28, 38, 52, 65, 77),
        15 -> List(30, 42, 55, 68, 80),
        16 -> List(32, 45, 58, 71, 83),
        17 -> List(35, 48, 62, 74, 86)
      )
      val edadRef = math.max(9, math.min(17, edad))
      val refs    = percentilesRef.getOrElse(edadRef, List(20, 35, 50, 65, 80))
      val percentil: Int =
        if (rawScore <= refs(0)) 5
        else if (rawScore <= refs(1)) 15
        else if (rawScore <= refs(2)) 35
        else if (rawScore <= refs(3)) 60
        else if (rawScore <= refs(4)) 80
        else 95

      // ── 7. Label de nivel formativo ─────────────────────────────────────────
      val nivelLabel: String =
        if (percentil >= 90) "ELITE NACIONAL"
        else if (percentil >= 75) "ACADEMIA PRIMERA"
        else if (percentil >= 50) "ACADEMIA REGIONAL"
        else if (percentil >= 25) "FORMATIVO MEDIO"
        else "EN DESARROLLO"

      val nivelColor: String =
        if (percentil >= 90) "success"
        else if (percentil >= 75) "info"
        else if (percentil >= 50) "warning"
        else "secondary"

      // ── 8. Análisis IA ──────────────────────────────────────────────────────
      val analisisIA: String = {
        val euroStr = if (valorEstimado >= 1000) s"${valorEstimado/1000}K€" else s"${valorEstimado}€"
        val prompt = s"""Eres un director de captacion de un club de LaLiga evaluando el perfil formativo de un portero.
Datos del portero (${edad} años):
- Nota media actuaciones: ${f"$notaMedia%.1f"}/100
- SPV (Sweeper Value): ${f"$spvEfic%.1f"}/100
- Bypass Rate eficiencia: ${f"${bypassEfic*100}%.0f"}%
- PSxG Delta: ${if(psxgDelta >= 0) "+" else ""}${f"$psxgDelta%.2f"} (positivo = mejor que xG esperado)
- Win Rate: ${f"${winRate*100}%.0f"}%
- Factor madurez bio-banding: $bioFactor
- Puntuacion modelo: ${f"$rawScore%.1f"}/100
- Valor estimado: $euroStr
- Percentil academias nacionales: $percentil%

Responde en espanol en exactamente 3 bloques HTML:
<h4>DIAGNOSTICO</h4><p>[perfil general en 2 frases]</p>
<h4>PALANCAS DE VALOR</h4><p>[las 2 metricas que mas elevan su valor y por que]</p>
<h4>RUTA AL SIGUIENTE NIVEL</h4><p>[que tiene que mejorar para subir un tier en los proximos 12 meses]</p>
Solo HTML limpio, sin markdown ni backticks."""
        AIProvider.ask(prompt, None, bypassCache = false)
      }

      // ── 9. Evolucion del rawScore por temporada ─────────────────────────────
      val rsEvo = conn.createStatement().executeQuery("""
        SELECT
          s.nombre_club as club, s.id as tid,
          COALESCE(AVG(m.nota), 0) as nota_t,
          COALESCE(AVG(m.paradas_1v1), 0) as p1v1_t,
          COALESCE(AVG(m.paradas_aereas), 0) as paer_t,
          COALESCE(AVG(m.paradas), 0) as par_t
        FROM seasons s
        LEFT JOIN matches m ON m.status='PLAYED'
          AND m.fecha BETWEEN COALESCE(s.fecha_inicio, '2000-01-01') AND COALESCE(s.fecha_fin, CURRENT_DATE)
        GROUP BY s.id, s.nombre_club ORDER BY s.id ASC
      """)
      var evoSeries = List[(String, Double)]()
      while (rsEvo.next()) {
        val nT   = rsEvo.getDouble("nota_t")
        val p1T  = rsEvo.getDouble("p1v1_t")
        val pAT  = rsEvo.getDouble("paer_t")
        val pT   = rsEvo.getDouble("par_t")
        val spvT = math.min(100.0, (p1T * 1.5 + pAT * 1.2 + (pT - p1T - pAT)) * 10.0)
        val nNorm = math.max(0, math.min(1.0, (nT - 40.0) / 60.0))
        val sNorm = math.max(0, math.min(1.0, spvT / 100.0))
        val scoreT = nNorm * 35.0 + sNorm * 20.0 + 50.0 * 0.45  // otras metricas sin historico
        val label  = Option(rsEvo.getString("club")).filter(_.nonEmpty).getOrElse(s"T${rsEvo.getInt("tid")}")
        evoSeries = evoSeries :+ (label, math.min(100.0, scoreT))
      }

      Map(
        "rawScore"      -> rawScore,
        "valorEstimado" -> valorEstimado,
        "percentil"     -> percentil,
        "nivelLabel"    -> nivelLabel,
        "nivelColor"    -> nivelColor,
        "notaMedia"     -> notaMedia,
        "spvEfic"       -> spvEfic,
        "bypassEfic"    -> bypassEfic,
        "psxgDelta"     -> psxgDelta,
        "winRate"       -> winRate,
        "bioFactor"     -> bioFactor,
        "edad"          -> edad,
        "pj"            -> pj,
        "limpias"       -> limpias,
        "analisisIA"    -> analisisIA,
        "evoLabels"     -> evoSeries.map(_._1),
        "evoScores"     -> evoSeries.map(_._2),
        "refs"          -> refs
      )
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // FASE 7 v7.2 — NLP SCOUTING AGGREGATOR
  // ─────────────────────────────────────────────────────────────────────────────
  def processScoutReport(
    textoRaw: String, ojeador: String, clubOrigen: String, fecha: String
  ): Map[String, Any] = {
    // Llamada a Gemini con prompt estructurado
    val prompt = s"""Eres un analista de captacion experto en porteros de formacion.
Has recibido el siguiente informe de un ojeador sobre un portero:

---
$textoRaw
---

Extrae y estructura la informacion en JSON PURO (sin markdown, sin backticks, sin explicaciones):
{
  "nivel_tecnico": <0-10>,
  "nivel_tactico": <0-10>,
  "nivel_fisico": <0-10>,
  "nivel_mental": <0-10>,
  "nivel_distribucion": <0-10>,
  "nivel_global": <0-10>,
  "proyeccion": "<ELITE|PRIMERA|SEGUNDA|REGIONAL|FORMATIVO>",
  "recomendacion": "<FICHAR_YA|SEGUIMIENTO_6M|SEGUIMIENTO_12M|DESCARTAR>",
  "fortalezas": "<lista de 2-3 puntos fuertes concretos en 1 linea>",
  "areas_mejora": "<lista de 2-3 areas de mejora concretas en 1 linea>",
  "resumen_ia": "<parrafo de 3-4 frases con el veredicto final del informe>"
}
SOLO el JSON, nada mas."""

    val respuesta = AIProvider.ask(prompt, None, bypassCache = true)

    // Parsear JSON de Gemini
    val cleaned = respuesta.replace("```json","").replace("```","").trim
    val parsed: ujson.Value = try { ujson.read(cleaned) }
                              catch { case _: Exception => ujson.Obj() }

    def jInt(k: String): Int    = try { parsed(k).num.toInt } catch { case _: Exception => 0 }
    def jStr(k: String): String = try { parsed(k).str }       catch { case _: Exception => "" }

    val nivTec = jInt("nivel_tecnico")
    val nivTac = jInt("nivel_tactico")
    val nivFis = jInt("nivel_fisico")
    val nivMen = jInt("nivel_mental")
    val nivDis = jInt("nivel_distribucion")
    val nivGlb = jInt("nivel_global")
    val proy   = jStr("proyeccion")
    val rec    = jStr("recomendacion")
    val fort   = jStr("fortalezas")
    val areas  = jStr("areas_mejora")
    val res    = jStr("resumen_ia")

    // Guardar en DB
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("""
        INSERT INTO scouting_reports
          (fecha, ojeador, club_origen, texto_raw, nivel_tecnico, nivel_tactico,
           nivel_fisico, nivel_mental, nivel_distribucion, nivel_global,
           proyeccion, recomendacion, fortalezas, areas_mejora, resumen_ia)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        RETURNING id
      """)
      val fechaDate = try { java.sql.Date.valueOf(fecha) }
                      catch { case _: Exception => java.sql.Date.valueOf(java.time.LocalDate.now().toString) }
      ps.setDate(1, fechaDate)
      ps.setString(2, fixEncoding(ojeador))
      ps.setString(3, fixEncoding(clubOrigen))
      ps.setString(4, fixEncoding(textoRaw))
      ps.setInt(5, nivTec); ps.setInt(6, nivTac); ps.setInt(7, nivFis)
      ps.setInt(8, nivMen); ps.setInt(9, nivDis); ps.setInt(10, nivGlb)
      ps.setString(11, proy); ps.setString(12, rec)
      ps.setString(13, fixEncoding(fort)); ps.setString(14, fixEncoding(areas))
      ps.setString(15, fixEncoding(res))
      val rs = ps.executeQuery()
      val newId = if (rs.next()) rs.getInt(1) else -1
      Map(
        "id"              -> newId,
        "nivel_tecnico"   -> nivTec, "nivel_tactico"     -> nivTac,
        "nivel_fisico"    -> nivFis, "nivel_mental"       -> nivMen,
        "nivel_distribucion" -> nivDis, "nivel_global"   -> nivGlb,
        "proyeccion"      -> proy,  "recomendacion"      -> rec,
        "fortalezas"      -> fort,  "areas_mejora"       -> areas,
        "resumen_ia"      -> res
      )
    } finally { conn.close() }
  }

  def getScoutReports(): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("""
        SELECT id, fecha, ojeador, club_origen, nivel_global, proyeccion,
               recomendacion, resumen_ia, fortalezas, areas_mejora,
               nivel_tecnico, nivel_tactico, nivel_fisico, nivel_mental, nivel_distribucion
        FROM scouting_reports ORDER BY fecha DESC, id DESC
      """)
      var rows = List[Map[String, Any]]()
      while (rs.next()) {
        rows = rows :+ Map(
          "id"           -> rs.getInt("id"),
          "fecha"        -> rs.getDate("fecha").toString,
          "ojeador"      -> Option(rs.getString("ojeador")).getOrElse(""),
          "club"         -> Option(rs.getString("club_origen")).getOrElse(""),
          "global"       -> rs.getInt("nivel_global"),
          "proyeccion"   -> Option(rs.getString("proyeccion")).getOrElse(""),
          "recomendacion"-> Option(rs.getString("recomendacion")).getOrElse(""),
          "resumen"      -> Option(rs.getString("resumen_ia")).getOrElse(""),
          "fortalezas"   -> Option(rs.getString("fortalezas")).getOrElse(""),
          "areas"        -> Option(rs.getString("areas_mejora")).getOrElse(""),
          "tec"          -> rs.getInt("nivel_tecnico"),
          "tac"          -> rs.getInt("nivel_tactico"),
          "fis"          -> rs.getInt("nivel_fisico"),
          "men"          -> rs.getInt("nivel_mental"),
          "dis"          -> rs.getInt("nivel_distribucion")
        )
      }
      rows
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // FASE 7 v7.2 — PERIODIZACION NUTRICIONAL REACTIVA
  // ─────────────────────────────────────────────────────────────────────────────
  def getNutritionPlan(forceRefresh: Boolean = false): Map[String, Any] = {
    val conn = getConnection()
    try {
      // ── 1. ACWR actual ──────────────────────────────────────────────────────
      val rsAg = conn.prepareStatement("SELECT COALESCE(SUM(rpe * 60), 0) FROM trainings WHERE fecha >= CURRENT_DATE - ?")
      rsAg.setInt(1, 7); val ag = rsAg.executeQuery(); val cargaAguda = if (ag.next()) ag.getDouble(1) else 0.0
      val rsCr = conn.prepareStatement("SELECT COALESCE(SUM(rpe * 60), 0) / 4.0 FROM trainings WHERE fecha >= CURRENT_DATE - ?")
      rsCr.setInt(1, 28); val cr = rsCr.executeQuery(); val cargaCronica = if (cr.next() && cr.getDouble(1) > 0) cr.getDouble(1) else 1.0
      val acwr: Double = cargaAguda / cargaCronica

      // ── 2. RPE media últimos 7 días ─────────────────────────────────────────
      val rsRpe = conn.createStatement().executeQuery(
        "SELECT COALESCE(AVG(rpe), 5.0) as rpe_m FROM trainings WHERE fecha >= CURRENT_DATE - 7")
      val rpeMedia: Double = if (rsRpe.next()) rsRpe.getDouble("rpe_m") else 5.0

      // ── 3. Nota último partido ──────────────────────────────────────────────
      val rsUlt = conn.createStatement().executeQuery(
        "SELECT nota, rival FROM matches WHERE status='PLAYED' ORDER BY fecha DESC LIMIT 1")
      val (notaUlt, rivalUlt) = if (rsUlt.next()) (rsUlt.getDouble("nota"), rsUlt.getString("rival")) else (60.0, "rival")

      // ── 4. Próximo partido ──────────────────────────────────────────────────
      val rsProx = conn.createStatement().executeQuery(
        "SELECT rival, fecha, tipo_partido FROM matches WHERE status='SCHEDULED' AND fecha >= CURRENT_DATE ORDER BY fecha ASC LIMIT 1")
      val (proximoRival, proximoFecha, proximoTipo) =
        if (rsProx.next()) (rsProx.getString("rival"),
          rsProx.getDate("fecha").toString,
          Option(rsProx.getString("tipo_partido")).getOrElse("LIGA"))
        else ("", "", "LIGA")

      // ── 5. Datos físicos actuales ───────────────────────────────────────────
      val rsFis = conn.createStatement().executeQuery(
        "SELECT altura, peso FROM physical_growth ORDER BY fecha DESC LIMIT 1")
      val (altura, peso) = if (rsFis.next()) (rsFis.getDouble("altura"), rsFis.getDouble("peso")) else (140.0, 35.0)

      // ── 6. Fase de carga: determinar contexto semanal ──────────────────────
      val faseStr: String =
        if (acwr > 1.5) "CARGA ALTA — semana de mucho trabajo o partido reciente"
        else if (acwr > 1.2) "CARGA MODERADA-ALTA — semana exigente"
        else if (acwr > 0.8) "CARGA NORMAL — semana standard"
        else "DESCARGA — semana de poco trabajo"

      val rendimientoStr: String =
        if (notaUlt >= 75) "buen rendimiento reciente (nota alta)"
        else if (notaUlt >= 55) "rendimiento normal"
        else "rendimiento bajo — posible fatiga o situacion de mejora"

      // ── 7. Verificar si hay plan reciente (< 6 días) en cache ─────────────
      if (!forceRefresh) {
        val rsCache = conn.createStatement().executeQuery(
          "SELECT plan_ia FROM nutrition_plans WHERE semana >= CURRENT_DATE - 6 ORDER BY created_at DESC LIMIT 1")
        if (rsCache.next()) {
          val cached = rsCache.getString("plan_ia")
          if (cached.nonEmpty && !cached.startsWith("Error")) {
            return Map("plan" -> cached, "acwr" -> acwr, "rpe" -> rpeMedia, "nota" -> notaUlt,
              "faseStr" -> faseStr, "altura" -> altura, "peso" -> peso, "cached" -> true)
          }
        }
      }

      // ── 8. Prompt a Gemini ──────────────────────────────────────────────────
      val proximoStr = if (proximoRival.nonEmpty) s"Tiene partido $proximoTipo contra $proximoRival el $proximoFecha."
                       else "No tiene partido programado esta semana."
      val prompt = s"""Eres un nutricionista deportivo especializado en porteros de formacion (academias de futbol).

Perfil del portero:
- Edad estimada: ${java.time.Period.between(
        try { val rs2 = conn.createStatement().executeQuery("SELECT fecha_nacimiento FROM seasons ORDER BY id DESC LIMIT 1")
              if (rs2.next()) java.time.LocalDate.parse(Option(rs2.getDate("fecha_nacimiento")).map(_.toString).getOrElse("2015-06-19"))
              else java.time.LocalDate.of(2015,6,19) }
        catch { case _:Exception => java.time.LocalDate.of(2015,6,19) },
        java.time.LocalDate.now()).getYears} años
- Altura: ${altura.toInt} cm | Peso: ${f"$peso%.1f"} kg
- Contexto de carga: ACWR = ${f"$acwr%.2f"} ($faseStr)
- RPE media ultimos 7 dias: ${f"$rpeMedia%.1f"}/10
- Ultimo partido: ${f"$notaUlt%.0f"}/100 ($rendimientoStr)
- $proximoStr

Genera un PLAN NUTRICIONAL SEMANAL REACTIVO en HTML limpio (sin markdown, sin backticks).
Estructura exacta:
<h4>DIAGNOSTICO DE CARGA</h4>
<p>[1 parrafo evaluando el estado energetico actual]</p>

<h4>MACROS RECOMENDADOS (diarios)</h4>
<div class="row g-2 mb-3">
  <div class="col-6 col-md-3"><div class="card bg-dark border-primary text-center p-2">
    <div class="h3 text-primary fw-bold">[X]g</div><div class="small text-muted">Proteina</div>
    <div class="xx-small text-secondary">[razon]</div>
  </div></div>
  [repite para Carbohidratos, Grasas Saludables, Hidratacion en litros]
</div>

<h4>DISTRIBUCION POR DIA</h4>
<p>[descripcion de los 3 tipos de dias de la semana: dia pre-partido, dia partido, dia recuperacion]</p>

<h4>ALIMENTOS CLAVE ESTA SEMANA</h4>
<ul>[3-5 alimentos especificos con su razon nutricional]</ul>

<h4>ALERTA NUTRICIONAL</h4>
<p>[1 aviso especifico basado en ACWR o estado actual]</p>

Adapta TODO al contexto real: si ACWR > 1.3 prioriza recuperacion; si ACWR < 0.8 prioriza carga. Si hay partido proximos dias, reajusta los carbohidratos.
Solo HTML limpio."""

      val planIA = AIProvider.ask(prompt, None, bypassCache = true)

      // ── 9. Guardar en cache ─────────────────────────────────────────────────
      val psSave = conn.prepareStatement(
        "INSERT INTO nutrition_plans (semana, acwr, rpe_media, nota_ultimo, plan_ia) VALUES (CURRENT_DATE, ?, ?, ?, ?)")
      psSave.setDouble(1, acwr); psSave.setDouble(2, rpeMedia)
      psSave.setDouble(3, notaUlt); psSave.setString(4, planIA)
      psSave.executeUpdate()

      Map("plan" -> planIA, "acwr" -> acwr, "rpe" -> rpeMedia, "nota" -> notaUlt,
          "faseStr" -> faseStr, "altura" -> altura, "peso" -> peso, "cached" -> false)
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // MODULO 1 — MOTOR DE PREDICCION DE TECHO
  // ─────────────────────────────────────────────────────────────────────────────
  def getTechoPrediction(forceRefresh: Boolean = false): Map[String, Any] = {
    val conn = getConnection()
    try {
      if (!forceRefresh) {
        val rsCache = conn.createStatement().executeQuery(
          "SELECT payload FROM feature_cache WHERE cache_key = 'techo_prediction' AND updated_at > NOW() - INTERVAL '30 days'"
        )
        if (rsCache.next()) {
          val json = ujson.read(rsCache.getString("payload"))
          return Map(
            "semaforo"    -> json("semaforo").str,
            "atributos"   -> json("atributos").arr.map(a => Map(
              "nombre" -> a("nombre").str, "actual" -> a("actual").num,
              "proy10" -> a("proy10").num, "proy14" -> a("proy14").num,
              "tendencia" -> a("tendencia").str, "flecha" -> a("flecha").str
            )).toList,
            "topAtributo" -> json("topAtributo").str,
            "analisisIA"  -> json("analisisIA").str,
            "edad"        -> json("edad").num.toInt
          )
        }
      }

      val card = getLatestCardData()
      val edad = calcularEdadExacta(card.fechaNacimiento)

      case class SeasonRow(fechaInicio: Option[String], div: Double, han: Double, kic: Double, ref: Double, spd: Double, pos: Double)
      val rsSeasons = conn.createStatement().executeQuery(
        "SELECT fecha_inicio, stat_div, stat_han, stat_kic, stat_ref, stat_spd, stat_pos FROM seasons ORDER BY id ASC"
      )
      var seasons = List[SeasonRow]()
      while (rsSeasons.next()) {
        seasons = seasons :+ SeasonRow(
          Option(rsSeasons.getDate("fecha_inicio")).map(_.toString),
          rsSeasons.getDouble("stat_div"), rsSeasons.getDouble("stat_han"), rsSeasons.getDouble("stat_kic"),
          rsSeasons.getDouble("stat_ref"), rsSeasons.getDouble("stat_spd"), rsSeasons.getDouble("stat_pos")
        )
      }

      if (seasons.isEmpty) {
        return Map("semaforo" -> "AMARILLO", "atributos" -> List.empty[Map[String, Any]],
          "topAtributo" -> "", "analisisIA" -> "Sin datos suficientes todavía", "edad" -> edad)
      }

      // Anos transcurridos entre la primera y la ultima temporada (minimo 1 para evitar division por cero)
      val yearsSpan: Double = {
        val fechas = seasons.flatMap(_.fechaInicio).flatMap(f => try Some(LocalDate.parse(f)) catch { case _: Exception => None })
        if (fechas.size >= 2) math.max(1.0, Period.between(fechas.min, fechas.max).toTotalMonths / 12.0) else 1.0
      }

      val attrNames = List("DIV", "HAN", "KIC", "REF", "SPD", "POS")
      def valuesOf(attr: String): List[Double] = attr match {
        case "DIV" => seasons.map(_.div); case "HAN" => seasons.map(_.han); case "KIC" => seasons.map(_.kic)
        case "REF" => seasons.map(_.ref); case "SPD" => seasons.map(_.spd); case _ => seasons.map(_.pos)
      }

      val atributos = attrNames.map { attr =>
        val vals = valuesOf(attr)
        val actual = vals.last
        val deltas = if (vals.size >= 2) vals.sliding(2).map(p => p(1) - p(0)).toList else List.empty[Double]
        val deltaPerYear = if (vals.size >= 2) (actual - vals.head) / yearsSpan else 0.0
        val tendencia =
          if (deltas.size < 2) "ESTABLE"
          else {
            val diff = deltas.last - deltas.init.last
            if (diff > 0.5) "ACELERANDO" else if (diff < -0.5) "DESACELERANDO" else "ESTABLE"
          }
        val flecha = tendencia match {
          case "ACELERANDO" => "↑↑"; case "DESACELERANDO" => "↓"; case _ => "→"
        }
        val proy10 = if (edad < 10) math.min(99.0, actual + deltaPerYear * (10 - edad)) else actual
        val proy14 = if (edad < 14) math.min(99.0, actual + deltaPerYear * (14 - edad)) else actual
        Map[String, Any]("nombre" -> attr, "actual" -> actual, "proy10" -> proy10, "proy14" -> proy14,
            "tendencia" -> tendencia, "flecha" -> flecha, "deltaPerYear" -> deltaPerYear)
      }

      val tendCounts = atributos.groupBy(_("tendencia").asInstanceOf[String]).view.mapValues(_.size).toMap
      val semaforo =
        if (tendCounts.getOrElse("ACELERANDO", 0) > tendCounts.getOrElse("DESACELERANDO", 0) &&
            tendCounts.getOrElse("ACELERANDO", 0) >= tendCounts.getOrElse("ESTABLE", 0)) "VERDE"
        else if (tendCounts.getOrElse("DESACELERANDO", 0) > tendCounts.getOrElse("ACELERANDO", 0)) "ROJO"
        else "AMARILLO"

      val topAtributo = atributos.maxBy(_("deltaPerYear").asInstanceOf[Double]).apply("nombre").asInstanceOf[String]

      val datosStr = atributos.map { a =>
        s"${a("nombre")}: actual ${f"${a("actual").asInstanceOf[Double]}%.1f"}, tendencia ${a("tendencia")}"
      }.mkString("; ")

      val prompt = s"""Eres analista de desarrollo de porteros de elite. Hector tiene $edad anos. Su evolucion de atributos por temporada es: $datosStr. Calcula: 1) Proyeccion estimada de cada atributo a los 10 y 14 anos manteniendo la tendencia actual, 2) Un semaforo global: VERDE si la aceleracion de mejora aumenta, AMARILLO si se mantiene, ROJO si desacelera, 3) El atributo con mayor potencial de crecimiento y por que. Responde en texto plano."""

      val analisisIA = AIProvider.ask(prompt)

      val payload = ujson.Obj(
        "semaforo" -> semaforo,
        "atributos" -> ujson.Arr(atributos.map { a =>
          ujson.Obj(
            "nombre" -> a("nombre").asInstanceOf[String], "actual" -> a("actual").asInstanceOf[Double],
            "proy10" -> a("proy10").asInstanceOf[Double], "proy14" -> a("proy14").asInstanceOf[Double],
            "tendencia" -> a("tendencia").asInstanceOf[String], "flecha" -> a("flecha").asInstanceOf[String]
          ): ujson.Value
        }: _*),
        "topAtributo" -> topAtributo, "analisisIA" -> analisisIA, "edad" -> edad
      )
      val upsert = conn.prepareStatement("""
        INSERT INTO feature_cache (cache_key, payload, updated_at) VALUES ('techo_prediction', ?, NOW())
        ON CONFLICT (cache_key) DO UPDATE SET payload = EXCLUDED.payload, updated_at = NOW()
      """)
      upsert.setString(1, ujson.write(payload))
      upsert.executeUpdate()

      Map("semaforo" -> semaforo, "atributos" -> atributos, "topAtributo" -> topAtributo,
          "analisisIA" -> analisisIA, "edad" -> edad)
    } finally { conn.close() }
  }

  def invalidateTechoCache(): Unit = {
    val conn = getConnection()
    try { conn.createStatement().executeUpdate("DELETE FROM feature_cache WHERE cache_key = 'techo_prediction'") }
    finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // MODULO 2 — MOTOR DE CONTEXTO AMBIENTAL CRUZADO
  // ─────────────────────────────────────────────────────────────────────────────
  def getContextPatterns(): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(s"""
        SELECT clima, es_local,
          CASE WHEN dias_descanso >= 5 THEN 'descansado' ELSE 'cargado' END as estado_descanso,
          AVG(nota) as nota_media, COUNT(*) as partidos
        FROM (
          SELECT m.nota, m.clima, m.es_local,
            ${DateUtils.daysBetweenSQL("m.fecha", "LAG(m.fecha) OVER (ORDER BY m.fecha)")} as dias_descanso
          FROM matches m WHERE m.status = 'PLAYED'
        ) sub
        WHERE dias_descanso IS NOT NULL
        GROUP BY clima, es_local, estado_descanso
        HAVING COUNT(*) >= 2
        ORDER BY nota_media DESC
      """)
      var list = List[Map[String, Any]]()
      while (rs.next()) {
        val esLocalObj = rs.getObject("es_local")
        val esLocalStr = if (esLocalObj == null) "N/D" else if (rs.getBoolean("es_local")) "Local" else "Visitante"
        list = list :+ Map(
          "clima"          -> Option(rs.getString("clima")).getOrElse("Sol"),
          "esLocal"        -> esLocalStr,
          "estadoDescanso" -> rs.getString("estado_descanso"),
          "notaMedia"      -> rs.getDouble("nota_media"),
          "partidos"       -> rs.getInt("partidos")
        )
      }
      list
    } finally { conn.close() }
  }

  // Lectura desde cache unicamente — nunca llama a Gemini en el render de pagina
  def getContextOptimoPhrase(): String = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT payload FROM feature_cache WHERE cache_key = 'context_optimo' AND updated_at > NOW() - INTERVAL '7 days'"
      )
      if (rs.next()) return ujson.read(rs.getString("payload"))("frase").str

      // Cache vacia o caducada: se dispara la generacion en un hilo de fondo y se devuelve un placeholder
      val thread = new Thread(() => {
        try { generateContextOptimoPhrase() } catch { case _: Exception => () }
      })
      thread.setDaemon(true)
      thread.start()
      "Analizando patrones de rendimiento..."
    } finally { conn.close() }
  }

  // Llamada real a Gemini — SOLO se invoca desde un hilo de fondo (getContextOptimoPhrase) o refresco explicito
  def generateContextOptimoPhrase(): String = {
    val conn = getConnection()
    try {
      val patrones = getContextPatterns()
      if (patrones.size < 2) return ""
      val top3 = patrones.take(3)
      val combosStr = top3.map { c =>
        s"${c("clima")}/${c("esLocal")}/${c("estadoDescanso")}: nota media ${f"${c("notaMedia").asInstanceOf[Double]}%.1f"} (${c("partidos")} partidos)"
      }.mkString("; ")

      val prompt = s"""Basandote en estos patrones de rendimiento de Hector: $combosStr, escribe en una frase el contexto optimo en que rinde mejor y el contexto donde mas sufre. Sin inventar - solo lo que muestran los datos."""

      val respuesta = AIProvider.ask(prompt)

      val payload = ujson.Obj("frase" -> respuesta)
      val upsert = conn.prepareStatement("""
        INSERT INTO feature_cache (cache_key, payload, updated_at) VALUES ('context_optimo', ?, NOW())
        ON CONFLICT (cache_key) DO UPDATE SET payload = EXCLUDED.payload, updated_at = NOW()
      """)
      upsert.setString(1, ujson.write(payload))
      upsert.executeUpdate()
      respuesta
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // MODULO 3 — DETECTOR DE VENTANAS SENSIBLES DE APRENDIZAJE
  // ─────────────────────────────────────────────────────────────────────────────
  // Relaciona cada ventana con palabras clave de habilidad/categoria del checklist
  private val windowSkillKeywords: Map[String, List[String]] = Map(
    "Coordinacion y equilibrio" -> List("caida", "equilibrio", "lateralidad", "posicion de pies"),
    "Velocidad de reaccion"     -> List("blocaje", "reflejo", "concentracion", "anticipacion"),
    "Tecnica con balon"         -> List("pie", "pase", "conduccion", "distribucion"),
    "Velocidad y agilidad"      -> List("sale a por", "1v1", "aereo", "salida"),
    "Fuerza relativa"           -> List("valentia", "duelo"),
    "Tactica colectiva"         -> List("voz", "manda", "liderazgo", "comunicacion", "posicionamiento")
  )

  def getActiveWindows(edadActual: Int): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val card = getLatestCardData()
      val birth = try LocalDate.parse(card.fechaNacimiento) catch { case _: Exception => LocalDate.now().minusYears(edadActual.toLong) }

      val rs = conn.prepareStatement(
        "SELECT * FROM development_windows WHERE edad_inicio <= ? AND edad_fin >= ? ORDER BY edad_fin ASC"
      )
      rs.setInt(1, edadActual); rs.setInt(2, edadActual)
      val rsRes = rs.executeQuery()
      val pendingSkills = getGoalkeeperSkills().filterNot(_.conseguido)

      var list = List[Map[String, Any]]()
      while (rsRes.next()) {
        val ventana = rsRes.getString("ventana")
        val edadFin = rsRes.getInt("edad_fin")
        val closingDate = birth.plusYears(edadFin.toLong + 1)
        val mesesRestantes = math.max(0L, Period.between(LocalDate.now(), closingDate).toTotalMonths)
        val urgente = mesesRestantes < 12

        val keywords = windowSkillKeywords.getOrElse(ventana, Nil)
        val pendientes = pendingSkills.filter { s =>
          val texto = (s.habilidad + " " + s.categoria).toLowerCase
          keywords.exists(k => texto.contains(k))
        }.map(s => Map[String, Any]("id" -> s.id, "habilidad" -> s.habilidad, "categoria" -> s.categoria))

        list = list :+ Map(
          "ventana"          -> ventana,
          "descripcion"      -> Option(rsRes.getString("descripcion")).getOrElse(""),
          "edadInicio"       -> rsRes.getInt("edad_inicio"),
          "edadFin"          -> edadFin,
          "mesesRestantes"   -> mesesRestantes,
          "urgente"          -> urgente,
          "skillsPendientes" -> pendientes
        )
      }
      list
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // MODULO 4 — SIMULADOR DE ESCENARIOS "¿QUE PASA SI?"
  // ─────────────────────────────────────────────────────────────────────────────
  def simulateScenario(deltaNota: Double, limpiasExtra: Int, sesionesExtra: Int, atributo: String, deltaAtributo: Int): Map[String, String] = {
    val card = getLatestCardData()
    val edad = calcularEdadExacta(card.fechaNacimiento)
    val matches = getMatchesList()
    val pj = matches.size
    val notaMedia = if (pj > 0) matches.map(_.nota).sum / pj else 0.0
    def gcOf(m: MatchLog): Int = m.resultado.split("-").lastOption.flatMap(_.trim.toIntOption).getOrElse(1)
    val limpias = matches.count(gcOf(_) == 0)
    val percentil = try getMarketEstimatorData().getOrElse("percentil", 0).asInstanceOf[Int] catch { case _: Exception => 0 }

    val hipotesisParts = scala.collection.mutable.ListBuffer[String]()
    if (deltaNota > 0) hipotesisParts += f"su nota media subiera $deltaNota%.1f puntos"
    if (limpiasExtra > 0) hipotesisParts += s"consiguiera $limpiasExtra porterias a cero mas esta temporada"
    if (sesionesExtra > 0) hipotesisParts += s"anadiera $sesionesExtra sesiones extra de academia al mes"
    if (deltaAtributo > 0 && atributo.nonEmpty) hipotesisParts += s"mejorara el atributo $atributo en $deltaAtributo puntos"
    val hipotesisStr = if (hipotesisParts.isEmpty) "no hubiera ningun cambio respecto a la situacion actual" else hipotesisParts.mkString(", ")

    val prompt = s"""Hector tiene actualmente: rating ${card.media}, nota media ${f"$notaMedia%.1f"}, $limpias limpias de $pj partidos, percentil estimado $percentil. Si consiguiera $hipotesisStr, cual seria el impacto estimado en: 1) Su rating FUT, 2) Su percentil de benchmarking, 3) El plazo para alcanzar sus objetivos de temporada? Se concreto con numeros estimados. Advierte si la hipotesis no es realista para su edad ($edad anos). Responde en texto plano, maximo 3 lineas por punto, con exactamente este formato:
RATING: <texto>
PERCENTIL: <texto>
PLAZO: <texto>"""

    // No se cachea: cada simulacion es bajo demanda (endpoint POST explicito)
    val respuesta = AIProvider.ask(prompt, None, bypassCache = true)

    def extractSection(resp: String, tag: String, nextTag: Option[String]): String = {
      val upper = resp.toUpperCase
      val startIdx = upper.indexOf(s"$tag:")
      if (startIdx < 0) return resp.trim
      val contentStart = startIdx + tag.length + 1
      val endIdx = nextTag.map(nt => upper.indexOf(s"$nt:", contentStart)).filter(_ >= 0).getOrElse(resp.length)
      resp.substring(contentStart, endIdx).trim
    }
    val ratingTxt    = extractSection(respuesta, "RATING", Some("PERCENTIL"))
    val percentilTxt = extractSection(respuesta, "PERCENTIL", Some("PLAZO"))
    val plazoTxt     = extractSection(respuesta, "PLAZO", None)

    Map("rating" -> ratingTxt, "percentil" -> percentilTxt, "plazo" -> plazoTxt, "hipotesis" -> hipotesisStr)
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // MODULO 5 — DIARIO NARRATIVO AUTOMATICO DE TEMPORADA
  // ─────────────────────────────────────────────────────────────────────────────
  private val mesesEs = Map(1 -> "enero", 2 -> "febrero", 3 -> "marzo", 4 -> "abril", 5 -> "mayo", 6 -> "junio",
    7 -> "julio", 8 -> "agosto", 9 -> "septiembre", 10 -> "octubre", 11 -> "noviembre", 12 -> "diciembre")

  def mesLabel(mes: String): String = {
    val parts = mes.split("-")
    if (parts.length == 2) {
      val anio = parts(0)
      val numMes = parts(1).toIntOption.getOrElse(1)
      s"${mesesEs.getOrElse(numMes, mes)} de $anio"
    } else mes
  }

  /** Carita de La Voz del Portero por mes (YYYY-MM -> 1..5), para la lista del diario. */
  def getCaritasPorMes(): Map[String, Int] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("SELECT TO_CHAR(fecha, 'YYYY-MM') as mes, motivacion_carita FROM voz_portero")
      Iterator.continually(rs).takeWhile(_.next()).map(r => r.getString("mes") -> r.getInt("motivacion_carita")).toMap
    } finally { conn.close() }
  }

  /**
   * Primer lunes de mes: asegura el diario del mes anterior (lo genera con Gemini si falta; se llama
   * desde las tareas programadas, nunca desde el render). Devuelve (mes, contenido) si existe.
   */
  def asegurarDiarioMesAnterior(): Option[(String, String)] = {
    val hoy = LocalDate.now()
    if (hoy.getDayOfWeek != java.time.DayOfWeek.MONDAY || hoy.getDayOfMonth > 7) return None
    val mes = hoy.minusMonths(1).toString.take(7)
    val contenido = generateMonthlyDiary(mes)
    if (contenido.isEmpty || contenido.startsWith("Error")) None else Some(mes -> contenido)
  }

  def getSeasonDiaryEntries(): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("SELECT * FROM season_diary ORDER BY mes DESC")
      var list = List[Map[String, Any]]()
      while (rs.next()) {
        list = list :+ Map(
          "id"                -> rs.getInt("id"),
          "mes"               -> rs.getString("mes"),
          "contenido"         -> rs.getString("contenido"),
          "generadoEn"        -> Option(rs.getTimestamp("generado_en")).map(_.toString).getOrElse(""),
          "partidosIncluidos" -> rs.getInt("partidos_incluidos"),
          "hitosIncluidos"    -> rs.getInt("hitos_incluidos")
        )
      }
      list
    } finally { conn.close() }
  }

  /** Diario narrativo del mes. Cache permanente: si ya existe no se regenera. Nunca guarda un error de la IA. */
  def generateMonthlyDiary(mes: String): String = {
    val conn = getConnection()
    try {
      val psExiste = conn.prepareStatement("SELECT contenido FROM season_diary WHERE mes = ?")
      psExiste.setString(1, mes)
      val rsExiste = psExiste.executeQuery()
      if (rsExiste.next()) return rsExiste.getString("contenido")

      // Partidos del mes
      val psM = conn.prepareStatement(
        "SELECT rival, nota, goles_favor, goles_contra, analisis_voz FROM matches WHERE status='PLAYED' AND TO_CHAR(fecha, 'YYYY-MM') = ? ORDER BY fecha ASC")
      psM.setString(1, mes)
      val rsM = psM.executeQuery()
      case class MatchMonth(rival: String, nota: Double, resultado: String, audio: String)
      var matches = List[MatchMonth]()
      while (rsM.next()) matches = matches :+ MatchMonth(
        fixEncoding(rsM.getString("rival")), rsM.getDouble("nota"),
        s"${rsM.getInt("goles_favor")}-${rsM.getInt("goles_contra")}",
        Option(rsM.getString("analisis_voz")).getOrElse("")
      )
      val partidosStr = if (matches.isEmpty) "Sin partidos este mes"
        else matches.map(m => s"vs ${m.rival} (${m.resultado}, nota ${f"${m.nota}%.1f"})").mkString("; ")

      // Habilidades del checklist marcadas ese mes
      val psH = conn.prepareStatement(
        "SELECT habilidad FROM goalkeeper_skills WHERE conseguido = TRUE AND TO_CHAR(fecha_conseguido, 'YYYY-MM') = ?")
      psH.setString(1, mes)
      val rsH = psH.executeQuery()
      var hitos = List[String]()
      while (rsH.next()) hitos = hitos :+ fixEncoding(rsH.getString("habilidad"))
      val hitosStr = if (hitos.isEmpty) "Sin hitos nuevos este mes" else hitos.mkString(", ")

      // Oportunidades registradas ese mes
      val psO = conn.prepareStatement(
        "SELECT tipo, club_o_entidad FROM opportunities WHERE TO_CHAR(fecha, 'YYYY-MM') = ?")
      psO.setString(1, mes)
      val rsO = psO.executeQuery()
      var opps = List[String]()
      while (rsO.next()) {
        val entidad = Option(rsO.getString("club_o_entidad")).getOrElse("")
        opps = opps :+ (if (entidad.nonEmpty) s"${rsO.getString("tipo")} (${fixEncoding(entidad)})" else rsO.getString("tipo"))
      }
      val oppsStr = if (opps.isEmpty) "Sin oportunidades registradas este mes" else opps.mkString(", ")

      // Audio-diario mas destacado del mes: el que mas se aleja de la nota media del mes
      val audios = matches.filter(_.audio.nonEmpty)
      val avgNota = if (matches.nonEmpty) matches.map(_.nota).sum / matches.size else 0.0
      val audioDestacado = if (audios.isEmpty) "" else audios.maxBy(m => math.abs(m.nota - avgNota)).audio

      // MODULO LA VOZ DEL PORTERO: la cita literal de Hector de ese mes, si existe, es oro para la narrativa
      val psVoz = conn.prepareStatement("SELECT respuesta_aprendizaje FROM voz_portero WHERE TO_CHAR(fecha, 'YYYY-MM') = ?")
      psVoz.setString(1, mes)
      val rsVoz = psVoz.executeQuery()
      val citaVoz = if (rsVoz.next()) fixEncoding(rsVoz.getString("respuesta_aprendizaje")) else ""
      val citaVozLinea = if (citaVoz.nonEmpty) s" Ese mes, cuando le preguntaron qué era lo que más le gustaba aprender, Héctor respondió: '$citaVoz'." else ""

      val mesLbl = mesLabel(mes)
      val edad = calcularEdadExacta(getLatestCardData().fechaNacimiento)
      def filas(sql: String)(f: java.sql.ResultSet => String): List[String] = {
        val ps = conn.prepareStatement(sql); ps.setString(1, mes)
        val rs = ps.executeQuery(); Iterator.continually(rs).takeWhile(_.next()).map(f).toList
      }
      // Porterias a cero y hitos del Legado del mes
      val porteriasCero = matches.count(_.resultado.endsWith("-0"))
      val hitosLegado = filas("SELECT descripcion FROM hitos_conseguidos WHERE TO_CHAR(fecha, 'YYYY-MM') = ?")(r => fixEncoding(r.getString("descripcion")))
      // ACWR medio del mes (misma serie diaria que el explorador de correlaciones)
      val acwrMes: Option[Double] = variablesCorrelacion.find(_._1 == "acwr").flatMap { case (_, _, q) =>
        val ps = conn.prepareStatement(s"SELECT AVG(v) as m FROM ($q) x WHERE TO_CHAR(d, 'YYYY-MM') = ? AND v IS NOT NULL")
        ps.setString(1, mes)
        val rs = ps.executeQuery()
        if (rs.next()) Option(rs.getObject("m")).map(_ => rs.getDouble("m")) else None
      }
      val acwrTxt = acwrMes.map(a => f"ACWR medio $a%.2f (${nivelACWR(a)._3.toLowerCase})").getOrElse("sin datos de carga suficientes")
      // Sueno
      val suenoTxt = filas("SELECT AVG(horas_sueno) as h, AVG(sueno_profundo_min) FILTER (WHERE sueno_profundo_min > 0) as p, COUNT(*) as n FROM wellness WHERE horas_sueno > 0 AND TO_CHAR(fecha, 'YYYY-MM') = ?") { r =>
        if (r.getInt("n") == 0) "sin registros de sueño"
        else f"${r.getDouble("h")}%.1f h de media en ${r.getInt("n")} noches registradas" + Option(r.getObject("p")).map(_ => f", ${r.getDouble("p")}%.0f min de sueño profundo").getOrElse("")
      }.headOption.getOrElse("sin registros de sueño")
      // La Voz del Portero: carita y respuestas literales
      val vozTxt = filas("SELECT motivacion_carita, respuesta_error, respuesta_aprendizaje FROM voz_portero WHERE TO_CHAR(fecha, 'YYYY-MM') = ?") { r =>
        s"motivación ${r.getInt("motivacion_carita")}/5; ante un error dijo: \"${fixEncoding(r.getString("respuesta_error"))}\"; sobre lo que más le gusta aprender dijo: \"${fixEncoding(r.getString("respuesta_aprendizaje"))}\""
      }.headOption.getOrElse("sin registro este mes")
      // Analisis de video del mes
      val videoTxt = filas("SELECT rival, video_analisis_ia FROM matches WHERE video_analisis_ia IS NOT NULL AND video_analisis_ia <> '' AND TO_CHAR(fecha, 'YYYY-MM') = ?") { r =>
        val sec = parseVideoAnalysisSections(r.getString("video_analisis_ia"))
        s"vs ${fixEncoding(r.getString("rival"))}: fuerte en ${sec.getOrElse("PUNTOS FUERTES", "").take(250)}; a mejorar ${sec.getOrElse("PUNTOS A MEJORAR", "").take(250)}"
      }.mkString(" | ")
      // Crecimiento y lesiones
      val crecimientoTxt = filas("SELECT altura, peso FROM physical_growth WHERE TO_CHAR(fecha, 'YYYY-MM') = ? ORDER BY fecha DESC LIMIT 1") { r =>
        Seq(Option(r.getObject("altura")).filter(_ => r.getDouble("altura") > 0).map(_ => f"${r.getDouble("altura")}%.0f cm"),
            Option(r.getObject("peso")).filter(_ => r.getDouble("peso") > 0).map(_ => f"${r.getDouble("peso")}%.1f kg")).flatten.mkString(", ")
      }.filter(_.nonEmpty).headOption.getOrElse("")
      val lesionesTxt = filas("SELECT COALESCE(NULLIF(tipo, ''), 'lesión') as t, COALESCE(zona, '') as z FROM injuries WHERE TO_CHAR(fecha_inicio, 'YYYY-MM') = ?") { r =>
        s"${fixEncoding(r.getString("t"))} ${fixEncoding(r.getString("z"))}".trim
      }.mkString(", ")

      val datosMes = List(
        s"Partidos: $partidosStr",
        s"Porterías a cero: $porteriasCero",
        s"Hitos del Legado: ${if (hitosLegado.isEmpty) "ninguno" else hitosLegado.mkString(", ")}",
        s"Habilidades conseguidas: $hitosStr",
        s"Oportunidades: $oppsStr",
        s"Carga: $acwrTxt",
        s"Sueño: $suenoTxt",
        s"La Voz del Portero: $vozTxt",
        if (videoTxt.nonEmpty) s"Análisis de vídeo: $videoTxt" else "",
        if (crecimientoTxt.nonEmpty) s"Crecimiento: $crecimientoTxt" else "",
        if (lesionesTxt.nonEmpty) s"Lesiones: $lesionesTxt" else "",
        if (audioDestacado.nonEmpty) s"Lo que contó tras un partido: ${audioDestacado.take(400)}" else ""
      ).filter(_.nonEmpty).mkString("\n")

      val promptNarrativo = s"""Eres un escritor que crea el diario deportivo mensual de Héctor, un portero de $edad años. Escribe un relato en tercera persona, en prosa literaria (no bullet points, no informe técnico), sobre cómo fue el mes de $mesLbl en la vida deportiva de Héctor. Usa los datos como base pero escribe como un narrador omnisciente que observa el desarrollo de un niño pequeño. Máximo 4-5 párrafos. Incluye detalles concretos de los datos pero escríbelos de forma narrativa, no estadística. Si Héctor dijo algo en La Voz del Portero, cítalo literalmente entre comillas. El tono es cálido, observacional y orientado al futuro. Nunca uses frases como 'según los datos' o 'las estadísticas muestran'. Datos del mes:
$datosMes"""

      // Cache permanente en season_diary (un registro por mes); un error de la IA no se guarda
      val contenido = AIProvider.ask(promptNarrativo, None, bypassCache = true).trim
      if (contenido.isEmpty || contenido.startsWith("Error")) return contenido

      val insert = conn.prepareStatement("""
        INSERT INTO season_diary (mes, contenido, generado_en, partidos_incluidos, hitos_incluidos, datos_mes)
        VALUES (?, ?, NOW(), ?, ?, ?)
        ON CONFLICT (mes) DO NOTHING
      """)
      insert.setString(1, mes); insert.setString(2, contenido)
      insert.setInt(3, matches.size); insert.setInt(4, hitos.size + hitosLegado.size); insert.setString(5, datosMes)
      insert.executeUpdate()

      contenido
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // MODULO 6 — COMPARATIVA TEMPORAL ENTRE EDADES
  // ─────────────────────────────────────────────────────────────────────────────
  def getTemporalComparison(): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("""
        SELECT s.id, s.categoria, s.media, s.stat_div, s.stat_han, s.stat_kic,
          s.stat_ref, s.stat_spd, s.stat_pos,
          COUNT(m.id) as partidos,
          COALESCE(AVG(m.nota), 0) as nota_media,
          SUM(CASE WHEN m.goles_contra = 0 THEN 1 ELSE 0 END) as limpias,
          COALESCE(AVG(m.goles_contra), 0) as gc_media
        FROM seasons s
        LEFT JOIN matches m ON m.season_id = s.id AND m.status = 'PLAYED'
        GROUP BY s.id, s.categoria, s.media, s.stat_div, s.stat_han, s.stat_kic, s.stat_ref, s.stat_spd, s.stat_pos
        ORDER BY s.id ASC
      """)
      var list = List[Map[String, Any]]()
      while (rs.next()) {
        list = list :+ Map(
          "id"        -> rs.getInt("id"),
          "categoria" -> Option(rs.getString("categoria")).getOrElse("Temp"),
          "media"     -> rs.getDouble("media"),
          "div" -> rs.getDouble("stat_div"), "han" -> rs.getDouble("stat_han"), "kic" -> rs.getDouble("stat_kic"),
          "ref" -> rs.getDouble("stat_ref"), "spd" -> rs.getDouble("stat_spd"), "pos" -> rs.getDouble("stat_pos"),
          "partidos"  -> rs.getInt("partidos"),
          "notaMedia" -> rs.getDouble("nota_media"),
          "limpias"   -> rs.getInt("limpias"),
          "gcMedia"   -> rs.getDouble("gc_media")
        )
      }
      list
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // MODULO 7 — RED DE CONTACTOS (MINI CRM)
  // ─────────────────────────────────────────────────────────────────────────────
  private def rowToContact(rs: java.sql.ResultSet): Contact = Contact(
    rs.getInt("id"), fixEncoding(rs.getString("nombre")), rs.getString("rol"),
    Option(rs.getString("club_o_entidad")).map(fixEncoding).getOrElse(""),
    Option(rs.getString("telefono")).getOrElse(""), Option(rs.getString("email")).getOrElse(""),
    Option(rs.getString("como_conocido")).map(fixEncoding).getOrElse(""),
    Option(rs.getDate("ultima_interaccion")).map(_.toString),
    Option(rs.getString("notas")).map(fixEncoding).getOrElse(""),
    Option(rs.getString("importancia")).getOrElse("MEDIA"),
    Option(rs.getTimestamp("created_at")).map(_.toString).getOrElse("")
  )

  def getContacts(): List[Contact] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("""
        SELECT * FROM contacts
        ORDER BY CASE importancia WHEN 'ALTA' THEN 1 WHEN 'MEDIA' THEN 2 ELSE 3 END,
                 ultima_interaccion DESC NULLS LAST
      """)
      var list = List[Contact]()
      while (rs.next()) list = list :+ rowToContact(rs)
      list
    } finally { conn.close() }
  }

  def getContactById(id: Int): Option[Contact] = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("SELECT * FROM contacts WHERE id = ?")
      ps.setInt(1, id)
      val rs = ps.executeQuery()
      if (rs.next()) Some(rowToContact(rs)) else None
    } finally { conn.close() }
  }

  def saveContact(nombre: String, rol: String, clubOEntidad: String, telefono: String, email: String,
                   comoConocido: String, notas: String, importancia: String): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("""
        INSERT INTO contacts (nombre, rol, club_o_entidad, telefono, email, como_conocido, notas, importancia)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?)
      """)
      ps.setString(1, fixEncoding(nombre)); ps.setString(2, rol)
      ps.setString(3, fixEncoding(clubOEntidad)); ps.setString(4, telefono); ps.setString(5, email)
      ps.setString(6, fixEncoding(comoConocido)); ps.setString(7, fixEncoding(notas)); ps.setString(8, importancia)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def registrarInteraccion(id: Int, fecha: String, nota: String): Unit = {
    val conn = getConnection()
    try {
      val fechaVal = if (fecha.nonEmpty) fecha else LocalDate.now().toString
      val ps = conn.prepareStatement("""
        UPDATE contacts SET ultima_interaccion = ?,
          notas = CASE WHEN notas IS NULL OR notas = '' THEN ? ELSE notas || E'\n' || ? END
        WHERE id = ?
      """)
      val entrada = s"[$fechaVal] ${fixEncoding(nota)}"
      ps.setDate(1, Date.valueOf(fechaVal))
      ps.setString(2, entrada); ps.setString(3, entrada)
      ps.setInt(4, id)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // MODULO 4 (sesion actual) — MAPA DE VISIBILIDAD Y EVENTOS CLAVE
  // ─────────────────────────────────────────────────────────────────────────────
  def getVisibilityEvents(): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("""
        SELECT v.*, c.nombre AS contacto_nombre
        FROM visibility_events v
        LEFT JOIN contacts c ON c.id = v.contact_id
        WHERE EXTRACT(YEAR FROM v.fecha) = EXTRACT(YEAR FROM CURRENT_DATE)
        ORDER BY v.fecha ASC
      """)
      var list = List[Map[String, Any]]()
      while (rs.next()) {
        val ojeadoresObj = rs.getObject("ojeadores_presentes")
        val ojeadoresOpt: Option[Boolean] = if (ojeadoresObj == null) None else Some(rs.getBoolean("ojeadores_presentes"))
        val contactIdObj = rs.getInt("contact_id")
        val contactIdOpt: Option[Int] = if (rs.wasNull()) None else Some(contactIdObj)
        list = list :+ Map(
          "id"                  -> rs.getInt("id"),
          "nombre"              -> fixEncoding(rs.getString("nombre")),
          "fecha"               -> rs.getDate("fecha").toString,
          "tipo"                -> rs.getString("tipo"),
          "organizador"         -> Option(rs.getString("organizador")).map(fixEncoding).getOrElse(""),
          "nivelVisibilidad"    -> rs.getString("nivel_visibilidad"),
          "participamos"        -> rs.getBoolean("participamos"),
          "ojeadoresPresentes"  -> ojeadoresOpt,
          "contactId"           -> contactIdOpt,
          "contactoNombre"      -> Option(rs.getString("contacto_nombre")).map(fixEncoding).getOrElse(""),
          "notas"               -> Option(rs.getString("notas")).map(fixEncoding).getOrElse("")
        )
      }
      list
    } finally { conn.close() }
  }

  def saveVisibilityEvent(nombre: String, fecha: String, tipo: String, organizador: String, nivelVisibilidad: String): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement(
        "INSERT INTO visibility_events (nombre, fecha, tipo, organizador, nivel_visibilidad) VALUES (?, ?::date, ?, ?, ?)"
      )
      ps.setString(1, fixEncoding(nombre)); ps.setString(2, fecha); ps.setString(3, tipo)
      ps.setString(4, fixEncoding(organizador)); ps.setString(5, nivelVisibilidad)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def updateVisibilityParticipamos(id: Int, participamos: Boolean): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("UPDATE visibility_events SET participamos = ? WHERE id = ?")
      ps.setBoolean(1, participamos); ps.setInt(2, id)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def updateVisibilityOjeadores(id: Int, ojeadoresPresentes: Boolean, contactId: Option[Int]): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("UPDATE visibility_events SET ojeadores_presentes = ?, contact_id = ? WHERE id = ?")
      ps.setBoolean(1, ojeadoresPresentes)
      contactId match {
        case Some(cid) => ps.setInt(2, cid)
        case None      => ps.setNull(2, java.sql.Types.INTEGER)
      }
      ps.setInt(3, id)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  // Llamada explicita a Gemini disparada por boton POST — nunca en el render de pagina
  def recommendVisibilityEvents(): String = {
    val events = getVisibilityEvents()
    if (events.isEmpty) return "No hay eventos registrados para recomendar."

    val card = getLatestCardData()
    val edad = calcularEdadExacta(card.fechaNacimiento)
    val upcoming = getUpcomingMatches().take(10)

    val eventosStr = events.map { e =>
      val nivel = e("nivelVisibilidad").asInstanceOf[String]
      val participamos = if (e("participamos").asInstanceOf[Boolean]) "YA CONFIRMADO" else "sin confirmar"
      s"${e("fecha")}: ${e("nombre")} (${e("tipo")}, visibilidad $nivel, $participamos)"
    }.mkString("; ")
    val partidosStr = if (upcoming.isEmpty) "Sin partidos programados todavía"
      else upcoming.map(m => s"${m.fecha} vs ${fixEncoding(m.rival)}").mkString("; ")

    val prompt = s"""Eres asesor de captación en fútbol base español. Héctor es portero de $edad años.
Eventos de visibilidad disponibles esta temporada: $eventosStr.
Calendario de partidos oficiales ya programados: $partidosStr.
Teniendo en cuenta el nivel actual de Héctor y su edad, sugiere cuáles eventos priorizar para maximizar la visibilidad ante ojeadores, evitando solapamientos con el calendario de partidos y sin sobrecargar su temporada. Responde en texto plano, en formato de lista breve, máximo 5 líneas."""

    AIProvider.ask(prompt, None, bypassCache = true)
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE B1 — Z-SCORE DE RENDIMIENTO POR CONTEXTO
  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE B3: seasonId=0 = historico completo (comportamiento anterior, sin cambios)
  def getZScoreRendimiento(seasonId: Int = 0): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val sf = seasonFilter(seasonId).replace("season_id", "m.season_id")
      val rs = conn.createStatement().executeQuery(s"""
        SELECT m.id, m.clima, m.es_local, m.nota,
          (SELECT COALESCE(SUM(CASE WHEN src = 0 THEN minutos * 4 ELSE 60 * rpe END), 0) / 7.0
             FROM ((SELECT minutos, 0 as rpe, 0 as src, fecha FROM matches WHERE status = 'PLAYED')
                   UNION ALL
                   (SELECT 0, rpe, 1, fecha FROM trainings)) loads
             WHERE fecha <= m.fecha AND ${DateUtils.daysBetweenSQL("m.fecha", "fecha")} < 7) as acute_load,
          (SELECT COALESCE(SUM(CASE WHEN src = 0 THEN minutos * 4 ELSE 60 * rpe END), 0) / 28.0
             FROM ((SELECT minutos, 0 as rpe, 0 as src, fecha FROM matches WHERE status = 'PLAYED')
                   UNION ALL
                   (SELECT 0, rpe, 1, fecha FROM trainings)) loads
             WHERE fecha <= m.fecha AND ${DateUtils.daysBetweenSQL("m.fecha", "fecha")} < 28) as chronic_load
        FROM matches m
        WHERE m.status = 'PLAYED' AND m.nota > 0 $sf
        ORDER BY m.fecha ASC
      """)
      case class MCtx(id: Int, clima: String, esLocal: Option[Boolean], nota: Double, acute: Double, chronic: Double) {
        def acwr: Double = if (chronic > 0) acute / chronic else 0.0
      }
      var lista = List[MCtx]()
      while (rs.next()) {
        val elObj = rs.getObject("es_local")
        val el: Option[Boolean] = if (elObj == null) None else Some(rs.getBoolean("es_local"))
        lista = lista :+ MCtx(rs.getInt("id"), Option(rs.getString("clima")).getOrElse("Sol"), el, rs.getDouble("nota"), rs.getDouble("acute_load"), rs.getDouble("chronic_load"))
      }

      if (lista.size < 15) return List.empty[Map[String, Any]]

      def acwrBucket(v: Double): String = if (v < 0.8) "BAJA" else if (v <= 1.2) "NORMAL" else "ALTA"
      def contextKey(m: MCtx): (String, String, String) = (m.clima, m.esLocal.map(b => if (b) "L" else "V").getOrElse("N"), acwrBucket(m.acwr))

      val grupos = lista.groupBy(contextKey)
      lista.flatMap { m =>
        val grp = grupos(contextKey(m))
        if (grp.size < 4) None
        else {
          val media = grp.map(_.nota).sum / grp.size
          val varianza = grp.map(x => math.pow(x.nota - media, 2)).sum / grp.size
          val stddev = math.sqrt(varianza)
          if (stddev <= 0.0001) None
          else Some(Map[String, Any]("matchId" -> m.id, "zScore" -> ((m.nota - media) / stddev), "contextCount" -> grp.size))
        }
      }
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE B2 — DETECTOR DE TENDENCIA (media movil 5 partidos)
  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE B3: seasonId=0 = historico completo (comportamiento anterior, sin cambios)
  def getTrendLOESS(seasonId: Int = 0): Map[String, Any] = {
    val matches = getMatchesList(seasonId) // ORDER BY fecha DESC
    val notasRecientesPrimero = matches.map(_.nota)
    if (notasRecientesPrimero.size < 20) return Map("activo" -> false)

    val ultimos5 = notasRecientesPrimero.take(5)
    val anteriores5 = notasRecientesPrimero.slice(5, 10)
    val mediaUltimos5 = ultimos5.sum / ultimos5.size
    val mediaAnteriores5 = if (anteriores5.nonEmpty) anteriores5.sum / anteriores5.size else mediaUltimos5
    val diferencia = mediaUltimos5 - mediaAnteriores5

    val tendencia = if (diferencia > 0.3) "POSITIVA" else if (diferencia < -0.3) "NEGATIVA" else "ESTABLE"

    Map(
      "activo"             -> true,
      "tendencia"          -> tendencia,
      "diferencia"         -> diferencia,
      "mediaUltimos5"      -> mediaUltimos5,
      "mediaAnteriores5"   -> mediaAnteriores5
    )
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE B3 — PREDICTOR DE NOTA PRE-PARTIDO (regresion lineal multiple, OLS)
  // ─────────────────────────────────────────────────────────────────────────────
  private case class ObsRegresion(diasDescanso: Double, acwr: Double, horasSueno: Double, esLocal: Double, diasDesdeAcademia: Double, nota: Double)

  private def fetchObsRegresion(): List[ObsRegresion] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(s"""
        SELECT m.nota,
          ${DateUtils.daysBetweenSQL("m.fecha", "LAG(m.fecha) OVER (ORDER BY m.fecha)")} as dias_descanso,
          COALESCE(m.es_local::int, 0) as es_local,
          (SELECT COALESCE(SUM(CASE WHEN src = 0 THEN minutos * 4 ELSE 60 * rpe END), 0) / 7.0
             FROM ((SELECT minutos, 0 as rpe, 0 as src, fecha FROM matches WHERE status = 'PLAYED')
                   UNION ALL
                   (SELECT 0, rpe, 1, fecha FROM trainings)) loads
             WHERE fecha <= m.fecha AND ${DateUtils.daysBetweenSQL("m.fecha", "fecha")} < 7) as acute_load,
          (SELECT COALESCE(SUM(CASE WHEN src = 0 THEN minutos * 4 ELSE 60 * rpe END), 0) / 28.0
             FROM ((SELECT minutos, 0 as rpe, 0 as src, fecha FROM matches WHERE status = 'PLAYED')
                   UNION ALL
                   (SELECT 0, rpe, 1, fecha FROM trainings)) loads
             WHERE fecha <= m.fecha AND ${DateUtils.daysBetweenSQL("m.fecha", "fecha")} < 28) as chronic_load,
          COALESCE((SELECT w.horas_sueno FROM wellness w WHERE ${DateUtils.daysBetweenSQL("m.fecha", "w.fecha")} = 1), 0) as horas_sueno,
          COALESCE((${DateUtils.daysBetweenSQL("m.fecha", "SELECT MAX(t.fecha) FROM trainings t WHERE t.tipo ILIKE '%academia%' AND t.fecha <= m.fecha")}), 999) as dias_desde_academia
        FROM matches m
        WHERE m.status = 'PLAYED' AND m.nota > 0
        ORDER BY m.fecha ASC
      """)
      var list = List[ObsRegresion]()
      while (rs.next()) {
        val diasDescansoObj = rs.getObject("dias_descanso")
        val diasDescanso = if (diasDescansoObj == null) 3.0 else rs.getDouble("dias_descanso")
        val acuteLoad = rs.getDouble("acute_load"); val chronicLoad = rs.getDouble("chronic_load")
        val acwr = if (chronicLoad > 0) acuteLoad / chronicLoad else 0.0
        val diasAcademiaRaw = rs.getInt("dias_desde_academia")
        val diasAcademia = if (diasAcademiaRaw > 60) 14.0 else diasAcademiaRaw.toDouble
        list = list :+ ObsRegresion(diasDescanso, acwr, rs.getDouble("horas_sueno"), rs.getInt("es_local").toDouble, diasAcademia, rs.getDouble("nota"))
      }
      list
    } finally { conn.close() }
  }

  // Resuelve beta = (X^T X)^-1 X^T y mediante eliminacion de Gauss-Jordan (sin dependencias externas)
  private def olsRegression(X: Array[Array[Double]], y: Array[Double]): Option[Array[Double]] = {
    val n = X.length; if (n == 0) return None
    val p = X(0).length
    // Xt X (p x p) y Xt y (p)
    val xtx = Array.ofDim[Double](p, p)
    val xty = Array.ofDim[Double](p)
    for (i <- 0 until p; j <- 0 until p) xtx(i)(j) = (0 until n).map(k => X(k)(i) * X(k)(j)).sum
    for (i <- 0 until p) xty(i) = (0 until n).map(k => X(k)(i) * y(k)).sum

    // Gauss-Jordan sobre matriz aumentada [xtx | xty]
    val aug = Array.tabulate(p, p + 1) { (i, j) => if (j < p) xtx(i)(j) else xty(i) }
    for (col <- 0 until p) {
      var pivotRow = col
      for (r <- col + 1 until p) if (math.abs(aug(r)(col)) > math.abs(aug(pivotRow)(col))) pivotRow = r
      if (math.abs(aug(pivotRow)(col)) < 1e-9) return None // singular
      val tmp = aug(col); aug(col) = aug(pivotRow); aug(pivotRow) = tmp
      val pivotVal = aug(col)(col)
      for (j <- 0 to p) aug(col)(j) = aug(col)(j) / pivotVal
      for (r <- 0 until p if r != col) {
        val factor = aug(r)(col)
        for (j <- 0 to p) aug(r)(j) = aug(r)(j) - factor * aug(col)(j)
      }
    }
    Some(Array.tabulate(p)(i => aug(i)(p)))
  }

  def getRendimientoPrediccion(diasDescanso: Int, acwr: Double, horasSueno: Double, esLocal: Boolean, diasDesdeAcademia: Int): Double = {
    val obs = fetchObsRegresion()
    if (obs.size < 30) return -1.0

    // Variables: [intercepto, diasDescanso, acwr, horasSueno, esLocal, diasDesdeAcademia]
    val X = obs.map(o => Array(1.0, o.diasDescanso, o.acwr, o.horasSueno, o.esLocal, o.diasDesdeAcademia)).toArray
    val y = obs.map(_.nota).toArray

    olsRegression(X, y) match {
      case None => -1.0
      case Some(beta) =>
        val xNuevo = Array(1.0, diasDescanso.toDouble, acwr, horasSueno, if (esLocal) 1.0 else 0.0, diasDesdeAcademia.toDouble)
        val pred = beta.zip(xNuevo).map { case (b, x) => b * x }.sum
        math.max(0.0, math.min(10.0, pred))
    }
  }

  // Coeficientes beta del modelo entrenado con los datos actuales — para explicar que factores influyen mas
  def getRendimientoPrediccionFactores(): Option[List[(String, Double)]] = {
    val obs = fetchObsRegresion()
    if (obs.size < 30) return None
    val X = obs.map(o => Array(1.0, o.diasDescanso, o.acwr, o.horasSueno, o.esLocal, o.diasDesdeAcademia)).toArray
    val y = obs.map(_.nota).toArray
    olsRegression(X, y).map { beta =>
      List(
        "Días de descanso" -> beta(1),
        "ACWR"              -> beta(2),
        "Horas de sueño"    -> beta(3),
        "Jugar local"       -> beta(4),
        "Días desde academia" -> beta(5)
      )
    }
  }

  // Recoge automaticamente los inputs del modelo (proximo partido, ACWR, sueno, academia)
  def getRendimientoPrediccionAuto(): Map[String, Any] = {
    val conn = getConnection()
    try {
      val rsUlt = conn.createStatement().executeQuery("SELECT fecha FROM matches WHERE status = 'PLAYED' ORDER BY fecha DESC LIMIT 1")
      val fechaUltimoPartido = if (rsUlt.next()) Some(rsUlt.getDate("fecha").toLocalDate) else None

      val rsProx = conn.createStatement().executeQuery("SELECT fecha, es_local FROM matches WHERE status = 'SCHEDULED' ORDER BY fecha ASC LIMIT 1")
      val (fechaObjetivo, esLocal) = if (rsProx.next()) {
        val fl = rsProx.getDate("fecha").toLocalDate
        val elObj = rsProx.getObject("es_local")
        (fl, if (elObj == null) false else rsProx.getBoolean("es_local"))
      } else (LocalDate.now(), false)

      val diasDescanso = fechaUltimoPartido
        .map(f => java.time.temporal.ChronoUnit.DAYS.between(f, fechaObjetivo).toInt)
        .filter(_ >= 0).getOrElse(7)

      val acute = getWorkloads(7); val chronic = getWorkloads(28)
      val acwr = StatsCalculator.calculateACWR(acute, chronic)

      val rsSueno = conn.createStatement().executeQuery("SELECT horas_sueno FROM wellness ORDER BY fecha DESC LIMIT 1")
      val horasSueno = if (rsSueno.next()) rsSueno.getDouble("horas_sueno") else 8.0

      val rsAcademia = conn.createStatement().executeQuery("SELECT MAX(fecha) as f FROM trainings WHERE tipo ILIKE '%academia%'")
      val diasDesdeAcademia = if (rsAcademia.next() && rsAcademia.getDate("f") != null)
        java.time.temporal.ChronoUnit.DAYS.between(rsAcademia.getDate("f").toLocalDate, fechaObjetivo).toInt
      else 14

      val prediccion = getRendimientoPrediccion(diasDescanso, acwr, horasSueno, esLocal, diasDesdeAcademia)
      if (prediccion < 0) return Map("activo" -> false)

      val factores = getRendimientoPrediccionFactores().getOrElse(Nil)
      val factorPositivo = factores.filter(_._2 > 0).sortBy(-_._2).headOption.map(_._1).getOrElse("—")
      val factorNegativo = factores.filter(_._2 < 0).sortBy(_._2).headOption.map(_._1).getOrElse("—")

      Map("activo" -> true, "prediccion" -> prediccion, "factorPositivo" -> factorPositivo, "factorNegativo" -> factorNegativo)
    } finally { conn.close() }
  }

  // Inputs base del predictor (mismo cálculo que usa getRendimientoPrediccionAuto, expuesto para los escenarios)
  private def getPrediccionInputsBase(): (Int, Double, Double, Boolean, Int) = {
    val conn = getConnection()
    try {
      val rsUlt = conn.createStatement().executeQuery("SELECT fecha FROM matches WHERE status = 'PLAYED' ORDER BY fecha DESC LIMIT 1")
      val fechaUltimoPartido = if (rsUlt.next()) Some(rsUlt.getDate("fecha").toLocalDate) else None

      val rsProx = conn.createStatement().executeQuery("SELECT fecha, es_local FROM matches WHERE status = 'SCHEDULED' ORDER BY fecha ASC LIMIT 1")
      val (fechaObjetivo, esLocal) = if (rsProx.next()) {
        val fl = rsProx.getDate("fecha").toLocalDate
        val elObj = rsProx.getObject("es_local")
        (fl, if (elObj == null) false else rsProx.getBoolean("es_local"))
      } else (LocalDate.now(), false)

      val diasDescanso = fechaUltimoPartido
        .map(f => java.time.temporal.ChronoUnit.DAYS.between(f, fechaObjetivo).toInt)
        .filter(_ >= 0).getOrElse(7)

      val acute = getWorkloads(7); val chronic = getWorkloads(28)
      val acwr = StatsCalculator.calculateACWR(acute, chronic)

      val rsSueno = conn.createStatement().executeQuery("SELECT horas_sueno FROM wellness ORDER BY fecha DESC LIMIT 1")
      val horasSueno = if (rsSueno.next()) rsSueno.getDouble("horas_sueno") else 8.0

      val rsAcademia = conn.createStatement().executeQuery("SELECT MAX(fecha) as f FROM trainings WHERE tipo ILIKE '%academia%'")
      val diasDesdeAcademia = if (rsAcademia.next() && rsAcademia.getDate("f") != null)
        java.time.temporal.ChronoUnit.DAYS.between(rsAcademia.getDate("f").toLocalDate, fechaObjetivo).toInt
      else 14

      (diasDescanso, acwr, horasSueno, esLocal, diasDesdeAcademia)
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE E — MARKOV CAREER PATHING: score actual sin Gemini (duplica solo la parte
  // matematica de getMarketEstimatorData — NUNCA se puede llamar a esa función completa
  // desde el render de pagina porque dispara AIProvider.ask).
  // ─────────────────────────────────────────────────────────────────────────────
  private def calcularMarketScoreActual(): (Double, String) = {
    val conn = getConnection()
    try {
      val rsBase = conn.createStatement().executeQuery("""
        SELECT
          COALESCE(AVG(nota), 0.0) AS nota_media,
          COALESCE(AVG(paradas), 0.0) AS par_media,
          COALESCE(AVG(paradas_1v1), 0.0) AS par1v1_media,
          COALESCE(AVG(paradas_aereas), 0.0) AS paer_media,
          COALESCE(AVG(CASE WHEN acciones_pie > 0
                    THEN lineas_superadas::FLOAT / acciones_pie END), 0.0) AS bypass_efic,
          COALESCE(
            SUM(CASE WHEN goles_favor > goles_contra THEN 1 ELSE 0 END)::FLOAT
            / NULLIF(COUNT(*), 0), 0.0) AS win_rate
        FROM matches WHERE status='PLAYED'
      """)
      var notaMedia = 0.0; var par1v1 = 0.0; var parAer = 0.0; var parMedia = 0.0
      var bypassEfic = 0.0; var winRate = 0.0
      if (rsBase.next()) {
        notaMedia = rsBase.getDouble("nota_media"); parMedia = rsBase.getDouble("par_media")
        par1v1 = rsBase.getDouble("par1v1_media"); parAer = rsBase.getDouble("paer_media")
        bypassEfic = rsBase.getDouble("bypass_efic"); winRate = rsBase.getDouble("win_rate")
      }

      val rsPsxg = conn.createStatement().executeQuery("""
        SELECT
          COALESCE(AVG(goles_contra), 0) as gc_media,
          COALESCE(AVG(
            CASE zona_goles
              WHEN '5' THEN 0.85 WHEN '4' THEN 0.65 WHEN '6' THEN 0.65
              WHEN '2' THEN 0.45 WHEN '8' THEN 0.45
              ELSE 0.30
            END
          ), 0) as xg_media
        FROM matches WHERE status='PLAYED' AND goles_contra > 0
      """)
      var psxgDelta = 0.0
      if (rsPsxg.next()) psxgDelta = rsPsxg.getDouble("xg_media") - rsPsxg.getDouble("gc_media")

      val rsEdad = conn.createStatement().executeQuery("SELECT fecha_nacimiento FROM seasons ORDER BY id DESC LIMIT 1")
      val fechaNac = if (rsEdad.next()) Option(rsEdad.getDate("fecha_nacimiento")).map(_.toString).getOrElse("2015-06-19") else "2015-06-19"
      val edad = java.time.Period.between(java.time.LocalDate.parse(fechaNac), java.time.LocalDate.now()).getYears
      val bioFactor: Double = if (edad <= 10) 0.70 else if (edad <= 12) 0.80
                              else if (edad <= 14) 0.90 else if (edad <= 16) 1.00 else 1.10

      val spvEfic: Double = {
        val total = par1v1 * 1.5 + parAer * 1.2 + (parMedia - par1v1 - parAer)
        if (total > 0) math.min(100.0, total * 10.0) else 0.0
      }

      val notaNorm: Double   = math.max(0, math.min(1.0, (notaMedia * 10.0 - 40.0) / 60.0))
      val spvNorm: Double    = math.max(0, math.min(1.0, spvEfic / 100.0))
      val bypassNorm: Double = math.max(0, math.min(1.0, bypassEfic))
      val psxgNorm: Double   = math.max(0, math.min(1.0, (psxgDelta + 2.0) / 4.0))
      val bioBoost: Double   = (bioFactor - 0.7) / 0.4

      val rawScore: Double = notaNorm*35.0 + spvNorm*20.0 + bypassNorm*15.0 + psxgNorm*15.0 + winRate*10.0 + bioBoost*5.0

      val percentilesRef: Map[Int, List[Int]] = Map(
        9  -> List(15, 22, 35, 48, 62), 10 -> List(18, 26, 38, 51, 65), 11 -> List(20, 29, 42, 55, 68),
        12 -> List(22, 32, 45, 58, 71), 13 -> List(25, 35, 48, 62, 74), 14 -> List(28, 38, 52, 65, 77),
        15 -> List(30, 42, 55, 68, 80), 16 -> List(32, 45, 58, 71, 83), 17 -> List(35, 48, 62, 74, 86)
      )
      val edadRef = math.max(9, math.min(17, edad))
      val refs = percentilesRef.getOrElse(edadRef, List(20, 35, 50, 65, 80))
      val percentil: Int =
        if (rawScore <= refs(0)) 5 else if (rawScore <= refs(1)) 15 else if (rawScore <= refs(2)) 35
        else if (rawScore <= refs(3)) 60 else if (rawScore <= refs(4)) 80 else 95

      val nivelKey: String =
        if (percentil >= 90) "ELITE_NACIONAL"
        else if (percentil >= 75) "ACADEMIA_PRIMERA"
        else if (percentil >= 50) "ACADEMIA_REGIONAL"
        else if (percentil >= 25) "FORMATIVO_MEDIO"
        else "EN_DESARROLLO"

      (rawScore, nivelKey)
    } finally { conn.close() }
  }

  def calcularMarkovPathway(): Option[Map[String, Any]] = {
    val conn = getConnection()
    val cerradas = try {
      val rs = conn.createStatement().executeQuery(
        "SELECT COALESCE(nombre, categoria, 'Temporada') as nombre, media FROM seasons WHERE fecha_fin IS NOT NULL ORDER BY id ASC")
      var l = List[(String, Double)]()
      while (rs.next()) l = l :+ (fixEncoding(rs.getString("nombre")), rs.getDouble("media"))
      l
    } finally { conn.close() }

    if (cerradas.size < 2) return None

    val estados = List("EN_DESARROLLO", "FORMATIVO_MEDIO", "ACADEMIA_REGIONAL", "ACADEMIA_PRIMERA", "ELITE_NACIONAL")
    val estadosLabel = Map(
      "EN_DESARROLLO" -> "EN DESARROLLO", "FORMATIVO_MEDIO" -> "FORMATIVO MEDIO",
      "ACADEMIA_REGIONAL" -> "ACADEMIA REGIONAL", "ACADEMIA_PRIMERA" -> "ACADEMIA PRIMERA",
      "ELITE_NACIONAL" -> "ÉLITE NACIONAL"
    )

    val (_, nivelActual) = calcularMarketScoreActual()
    val idxActual = math.max(0, estados.indexOf(nivelActual))
    val siguienteEstadoKey = if (idxActual < estados.size - 1) Some(estados(idxActual + 1)) else None

    // Velocidad de mejora: puntos de rating FUT ganados por temporada (pendiente simple primera->ultima)
    val primerRating = cerradas.head._2
    val ultimoRating = cerradas.last._2
    val temporadasTranscurridas = math.max(1, cerradas.size - 1)
    val velocidadMejora = (ultimoRating - primerRating) / temporadasTranscurridas

    val probabilidad2Temp: Int =
      if (velocidadMejora > 5) 70
      else if (velocidadMejora >= 2) 45
      else 20

    // Gap tipico entre niveles formativos (aproximacion — el modelo es deliberadamente simple)
    val temporadasHastaSiguiente: Option[Double] =
      if (siguienteEstadoKey.isEmpty || velocidadMejora <= 0) None
      else Some(8.0 / velocidadMejora)

    val anioActual = LocalDate.now().getYear
    val temporadaEstimadaStr = temporadasHastaSiguiente.map(t => (anioActual + math.ceil(t).toInt).toString)

    Some(Map(
      "estadoActual"             -> estadosLabel.getOrElse(nivelActual, nivelActual),
      "siguienteEstado"          -> siguienteEstadoKey.map(s => estadosLabel.getOrElse(s, s)).getOrElse(""),
      "probabilidad2Temp"        -> probabilidad2Temp,
      "velocidadMejora"          -> velocidadMejora,
      "temporadasHastaSiguiente" -> temporadasHastaSiguiente,
      "temporadaEstimada"        -> temporadaEstimadaStr.getOrElse(""),
      "nTemporadas"              -> cerradas.size,
      "estados"                  -> estados.map(s => estadosLabel.getOrElse(s, s)),
      "estadoActualIdx"          -> idxActual
    ))
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE D — SIMULADOR WHAT-IF MEJORADO: escenarios predefinidos
  // ─────────────────────────────────────────────────────────────────────────────
  def simularEscenario(tipo: String): Map[String, Any] = {
    val obs = fetchObsRegresion()
    // El modelo de regresion (Prompt 1) exige >=30 observaciones para ser fiable
    if (obs.size < 30) return Map("activo" -> false, "partidosDisponibles" -> obs.size)

    val notaActualMedia = {
      val conn = getConnection()
      try {
        val rs = conn.createStatement().executeQuery("SELECT AVG(nota) as m FROM matches WHERE status='PLAYED' AND nota > 0")
        if (rs.next()) rs.getDouble("m") else 0.0
      } finally { conn.close() }
    }

    val (diasDescansoBase, acwrBase, horasSuenoBase, esLocalBase, diasAcademiaBase) = getPrediccionInputsBase()

    val escenario: Option[(Int, Double, Double, Int, String, String, String)] = tipo match {
      case "SUENO_MEJORADO" => Some((
        diasDescansoBase, acwrBase, horasSuenoBase + 0.5, diasAcademiaBase,
        "Sueño profundo", "😴 Sueño Mejorado",
        "Mejorar el sueño profundo 30 min/noche subiría la nota media de %.1f a %.1f en 10 semanas"
      ))
      case "ACWR_OPTIMO" => Some((
        diasDescansoBase, 0.9, horasSuenoBase, diasAcademiaBase,
        "ACWR óptimo (0.8-1.0)", "⚖️ ACWR Óptimo",
        "Mantener el ACWR siempre entre 0.8 y 1.0 subiría la nota media de %.1f a %.1f en 10 semanas"
      ))
      case "ACADEMIA_EXTRA" => Some((
        diasDescansoBase, acwrBase, horasSuenoBase, math.max(3, diasAcademiaBase - 7),
        "Sesión de academia extra", "🥅 Academia Extra",
        "Añadir una sesión de academia extra al mes subiría la nota media de %.1f a %.1f en 10 semanas"
      ))
      case "DESCANSO_OPTIMO" => Some((
        3, acwrBase, horasSuenoBase, diasAcademiaBase,
        "2-3 días de descanso antes del partido", "🛌 Descanso Óptimo",
        "Tener siempre 2-3 días de descanso antes del partido subiría la nota media de %.1f a %.1f en 10 semanas"
      ))
      case _ => None
    }

    escenario match {
      case None => Map("activo" -> false)
      case Some((diasDescansoEsc, acwrEsc, horasSuenoEsc, diasAcademiaEsc, factorTxt, nombreTxt, fraseTpl)) =>
        val notaProyectada = getRendimientoPrediccion(diasDescansoEsc, acwrEsc, horasSuenoEsc, esLocalBase, diasAcademiaEsc)
        if (notaProyectada < 0) Map("activo" -> false)
        else {
          val diferencia = notaProyectada - notaActualMedia
          Map(
            "activo" -> true, "tipo" -> tipo, "nombre" -> nombreTxt,
            "notaActual" -> notaActualMedia, "notaProyectada" -> notaProyectada, "diferencia" -> diferencia,
            "factor" -> factorTxt, "frase" -> fraseTpl.format(notaActualMedia, notaProyectada)
          )
        }
    }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE B4 — ALERTAS ESTADISTICAS PERSONALIZADAS (umbrales relativos a Hector)
  // ─────────────────────────────────────────────────────────────────────────────
  private def media(xs: List[Double]): Double = if (xs.isEmpty) 0.0 else xs.sum / xs.size
  private def stddev(xs: List[Double]): Double = {
    if (xs.size < 2) 0.0 else {
      val m = media(xs)
      math.sqrt(xs.map(x => math.pow(x - m, 2)).sum / xs.size)
    }
  }

  // BLOQUE B3: seasonId solo afecta a la parte de "nota de partido" (item 1) — el resto de
  // baselines (sueno, ACWR, wellness) son fisiologicos y deben seguir usando todo el historico.
  def getAlertasEstadisticasPersonales(seasonId: Int = 0): List[String] = {
    val conn = getConnection()
    try {
      var alertas = List[String]()

      // 1. Nota de partido: historico completo vs ultimo partido
      val matches = getMatchesList(seasonId) // DESC por fecha
      val notasHist = matches.map(_.nota)
      if (notasHist.size >= 20) {
        val mu = media(notasHist); val sd = stddev(notasHist)
        val ultimaNota = notasHist.head
        if (sd > 0 && ultimaNota < mu - 1.5 * sd)
          alertas = alertas :+ "⚠️ La última nota está significativamente por debajo de su patrón habitual"
      }

      // 2. Sueno profundo: historico completo vs media de la ultima semana
      val rsSp = conn.createStatement().executeQuery(
        "SELECT sueno_profundo_min, fecha FROM wellness WHERE sueno_profundo_min IS NOT NULL ORDER BY fecha ASC"
      )
      var spHist = List[Double]()
      while (rsSp.next()) spHist = spHist :+ rsSp.getDouble("sueno_profundo_min")
      if (spHist.size >= 20) {
        val mu = media(spHist); val sd = stddev(spHist)
        val rsRecent = conn.createStatement().executeQuery(
          "SELECT AVG(sueno_profundo_min) as m FROM wellness WHERE sueno_profundo_min IS NOT NULL AND fecha >= CURRENT_DATE - 7"
        )
        val recentAvg = if (rsRecent.next()) rsRecent.getDouble("m") else 0.0
        if (sd > 0 && recentAvg > 0 && recentAvg < mu - 1.5 * sd)
          alertas = alertas :+ "💤 El sueño profundo de esta semana está muy por debajo de su patrón normal"
      }

      // 3. ACWR: historico (aproximado con cargas semanales) vs actual
      val rsCargas = conn.createStatement().executeQuery("""
        SELECT TO_CHAR(fecha, 'IYYY-IW') as semana, SUM(carga) as carga_semana FROM (
          (SELECT fecha, minutos * 4 as carga FROM matches WHERE status = 'PLAYED')
          UNION ALL
          (SELECT fecha, 60 * rpe as carga FROM trainings)
        ) t GROUP BY semana ORDER BY semana ASC
      """)
      var cargasSemanales = List[Double]()
      while (rsCargas.next()) cargasSemanales = cargasSemanales :+ rsCargas.getDouble("carga_semana")
      if (cargasSemanales.size >= 20) {
        val acuteNow = getWorkloads(7).sum / 7.0
        val chronicNow = getWorkloads(28).sum / 28.0
        val acwrNow = if (chronicNow > 0) acuteNow / chronicNow else 0.0
        // Distribucion historica de ACWR semanal aproximada: carga_semana_i / media movil de 4 semanas previas
        var acwrHist = List[Double]()
        for (i <- 3 until cargasSemanales.size) {
          val chronicPrev = cargasSemanales.slice(i - 3, i + 1).sum / 4.0
          if (chronicPrev > 0) acwrHist = acwrHist :+ (cargasSemanales(i) / chronicPrev)
        }
        if (acwrHist.size >= 20) {
          val mu = media(acwrHist); val sd = stddev(acwrHist)
          if (sd > 0 && acwrNow > mu + 1.5 * sd)
            alertas = alertas :+ "🔴 La carga esta semana es inusualmente alta para Héctor — considera reducirla"
        }
      }

      // 4. GC por partido: historico vs ultimo
      val gcHist = matches.map(m => m.resultado.split("-").lastOption.flatMap(_.trim.toIntOption).getOrElse(0).toDouble)
      if (gcHist.size >= 20) {
        val mu = media(gcHist); val sd = stddev(gcHist)
        val ultimoGc = gcHist.head
        if (sd > 0 && ultimoGc > mu + 1.5 * sd)
          alertas = alertas :+ "🥅 Encajó significativamente más goles de lo habitual — revisar si fue el equipo o la actuación"
      }

      alertas
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE C3 — TEST FISICOS TRIMESTRALES
  // ─────────────────────────────────────────────────────────────────────────────
  def savePhysicalTest(fecha: String, velocidad10m: Option[Double], velocidad30m: Option[Double],
                        saltoVertical: Option[Int], agilidadIllinois: Option[Double],
                        lanzamientoMedicinal: Option[Int], notas: String): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement(
        "INSERT INTO physical_tests (fecha, velocidad_10m, velocidad_30m, salto_vertical_cm, agilidad_illinois_s, lanzamiento_medicinal_cm, notas) VALUES (?::date,?,?,?,?,?,?)"
      )
      ps.setString(1, if (fecha.nonEmpty) fecha else LocalDate.now().toString)
      def setOptD(idx: Int, v: Option[Double]): Unit = v match { case Some(x) => ps.setDouble(idx, x); case None => ps.setNull(idx, java.sql.Types.DOUBLE) }
      def setOptI(idx: Int, v: Option[Int]): Unit = v match { case Some(x) => ps.setInt(idx, x); case None => ps.setNull(idx, java.sql.Types.INTEGER) }
      setOptD(2, velocidad10m); setOptD(3, velocidad30m); setOptI(4, saltoVertical); setOptD(5, agilidadIllinois); setOptI(6, lanzamientoMedicinal)
      ps.setString(7, fixEncoding(notas))
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def getPhysicalTests(): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("SELECT * FROM physical_tests ORDER BY fecha ASC")
      var list = List[Map[String, Any]]()
      while (rs.next()) {
        def optD(col: String): Option[Double] = { val v = rs.getDouble(col); if (rs.wasNull()) None else Some(v) }
        def optI(col: String): Option[Int] = { val v = rs.getInt(col); if (rs.wasNull()) None else Some(v) }
        list = list :+ Map[String, Any](
          "fecha" -> rs.getDate("fecha").toString,
          "velocidad10m" -> optD("velocidad_10m"),
          "velocidad30m" -> optD("velocidad_30m"),
          "saltoVertical" -> optI("salto_vertical_cm"),
          "agilidadIllinois" -> optD("agilidad_illinois_s"),
          "lanzamientoMedicinal" -> optI("lanzamiento_medicinal_cm")
        )
      }
      list
    } finally { conn.close() }
  }

  // Lectura desde cache unicamente — nunca llama a Gemini en el render de pagina
  def getPhysicalTestsAnalysisCached(): Option[String] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT payload FROM feature_cache WHERE cache_key = 'physical_tests_ia' AND updated_at > NOW() - INTERVAL '30 days'"
      )
      if (rs.next()) Some(ujson.read(rs.getString("payload"))("analisis").str) else None
    } finally { conn.close() }
  }

  // Llamada real a Gemini — SOLO se invoca desde el boton POST "Analisis IA"
  def generatePhysicalTestsAnalysis(): String = {
    val conn = getConnection()
    try {
      val tests = getPhysicalTests()
      if (tests.isEmpty) return "Sin tests físicos registrados todavía."

      val card = getLatestCardData()
      val edad = calcularEdadExacta(card.fechaNacimiento)
      val fasePhv = try getDigitalTwinData(0, 0).getOrElse("fasePhv", "PRE-PICO").toString catch { case _: Exception => "PRE-PICO" }

      val datosStr = tests.map { t =>
        val v10 = t("velocidad10m").asInstanceOf[Option[Double]].map(v => f"$v%.2fs").getOrElse("—")
        val v30 = t("velocidad30m").asInstanceOf[Option[Double]].map(v => f"$v%.2fs").getOrElse("—")
        val sv = t("saltoVertical").asInstanceOf[Option[Int]].map(v => s"${v}cm").getOrElse("—")
        val ag = t("agilidadIllinois").asInstanceOf[Option[Double]].map(v => f"$v%.2fs").getOrElse("—")
        val lm = t("lanzamientoMedicinal").asInstanceOf[Option[Int]].map(v => s"${v}cm").getOrElse("—")
        s"${t("fecha")}: 10m=$v10, 30m=$v30, salto=$sv, illinois=$ag, lanzamiento=$lm"
      }.mkString("; ")

      val prompt = s"""Analiza la evolución de los test físicos de Héctor, portero de $edad años, en estado madurativo $fasePhv: [$datosStr]. Evalúa: 1) Qué capacidad física mejora más rápido, 2) Cuál está estancada o empeora, 3) Si la evolución es coherente con su fase de maduración. Máximo 3 líneas por punto."""

      val analisis = AIProvider.ask(prompt, None, bypassCache = true)
      val payload = ujson.Obj("analisis" -> analisis)
      val upsert = conn.prepareStatement("""
        INSERT INTO feature_cache (cache_key, payload, updated_at) VALUES ('physical_tests_ia', ?, NOW())
        ON CONFLICT (cache_key) DO UPDATE SET payload = EXCLUDED.payload, updated_at = NOW()
      """)
      upsert.setString(1, ujson.write(payload))
      upsert.executeUpdate()
      analisis
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE D2 — REGISTRO PSICOLOGICO TRIMESTRAL
  // ─────────────────────────────────────────────────────────────────────────────
  def savePsychRecord(fecha: String, motivacion: Int, presionPercibida: Int, relacionErrores: Int,
                       miedoFracaso: Int, disfrute: Int, relacionEntrenador: Int, relacionEquipo: Int, notas: String): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("""
        INSERT INTO psych_records (fecha, motivacion, presion_percibida, relacion_errores, miedo_fracaso, disfrute, relacion_entrenador, relacion_equipo, notas)
        VALUES (?::date,?,?,?,?,?,?,?,?)
      """)
      ps.setString(1, if (fecha.nonEmpty) fecha else LocalDate.now().toString)
      ps.setInt(2, motivacion); ps.setInt(3, presionPercibida); ps.setInt(4, relacionErrores)
      ps.setInt(5, miedoFracaso); ps.setInt(6, disfrute); ps.setInt(7, relacionEntrenador); ps.setInt(8, relacionEquipo)
      ps.setString(9, fixEncoding(notas))
      ps.executeUpdate()
    } finally { conn.close() }
  }

  def getPsychRecords(): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("SELECT * FROM psych_records ORDER BY fecha ASC")
      var list = List[Map[String, Any]]()
      while (rs.next()) {
        list = list :+ Map[String, Any](
          "fecha" -> rs.getDate("fecha").toString,
          "motivacion" -> rs.getInt("motivacion"),
          "presionPercibida" -> rs.getInt("presion_percibida"),
          "relacionErrores" -> rs.getInt("relacion_errores"),
          "miedoFracaso" -> rs.getInt("miedo_fracaso"),
          "disfrute" -> rs.getInt("disfrute"),
          "relacionEntrenador" -> rs.getInt("relacion_entrenador"),
          "relacionEquipo" -> rs.getInt("relacion_equipo")
        )
      }
      list
    } finally { conn.close() }
  }

  // Alerta si los 2 ultimos registros tienen motivacion < 3
  def getAlertaMotivacionBaja(): Boolean = {
    val records = getPsychRecords()
    val ultimos2 = records.takeRight(2)
    ultimos2.size == 2 && ultimos2.forall(_("motivacion").asInstanceOf[Int] < 3)
  }

  // Lectura desde cache unicamente — nunca llama a Gemini en el render de pagina
  def getPsychAnalysisCached(): Option[String] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT payload FROM feature_cache WHERE cache_key = 'psych_analysis' AND updated_at > NOW() - INTERVAL '30 days'"
      )
      if (rs.next()) Some(ujson.read(rs.getString("payload"))("analisis").str) else None
    } finally { conn.close() }
  }

  // Llamada real a Gemini — SOLO se invoca desde el boton POST "Analisis IA"
  def generatePsychAnalysis(): String = {
    val conn = getConnection()
    try {
      val records = getPsychRecords()
      if (records.isEmpty) return "Sin registros psicológicos todavía."

      val card = getLatestCardData()
      val edad = calcularEdadExacta(card.fechaNacimiento)
      val datosStr = records.map { r =>
        s"${r("fecha")}: motivación=${r("motivacion")}, presión=${r("presionPercibida")}, relación con errores=${r("relacionErrores")}, miedo al fracaso=${r("miedoFracaso")}, disfrute=${r("disfrute")}, relación entrenador=${r("relacionEntrenador")}, relación equipo=${r("relacionEquipo")}"
      }.mkString("; ")

      val prompt = s"""Analiza el perfil psicológico longitudinal de Héctor, futbolista de $edad años: [$datosStr]. Detecta: 1) Si hay señales de burnout o presión excesiva, 2) Su dimensión psicológica más fuerte y más débil, 3) Una recomendación concreta para el padre sobre cómo acompañarle en los próximos meses. Lenguaje empático, no alarmista, orientado a la acción. Máximo 3 líneas por punto."""

      val analisis = AIProvider.ask(prompt, None, bypassCache = true)
      val payload = ujson.Obj("analisis" -> analisis)
      val upsert = conn.prepareStatement("""
        INSERT INTO feature_cache (cache_key, payload, updated_at) VALUES ('psych_analysis', ?, NOW())
        ON CONFLICT (cache_key) DO UPDATE SET payload = EXCLUDED.payload, updated_at = NOW()
      """)
      upsert.setString(1, ujson.write(payload))
      upsert.executeUpdate()
      analisis
    } finally { conn.close() }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // BLOQUE D3 — HORAS DE PRACTICA DELIBERADA
  // ─────────────────────────────────────────────────────────────────────────────
  // No hay duracion registrada por sesion de entrenamiento: se asume una sesion tipo de 1.5h.
  private val horasPorSesionTrainingDefault = 1.5

  private def pesoTipoPractica(tipo: String): Double = {
    val t = tipo.toLowerCase
    if (t.contains("academia")) 1.0
    else if (t.contains("partido")) 1.2
    else if (t.contains("equipo") || t.contains("club")) 0.6
    else if (t.contains("judo")) 0.4
    else 0.5
  }

  private def categoriaPractica(tipo: String): String = {
    val t = tipo.toLowerCase
    if (t.contains("academia")) "Academia"
    else if (t.contains("partido")) "Partido"
    else if (t.contains("judo")) "Judo"
    else if (t.contains("papa")) "Papá/Portero"
    else "Club/Equipo"
  }

  def getHorasPracticaDeliberada(): Map[String, Any] = {
    val conn = getConnection()
    try {
      case class Sesion(fecha: String, tipo: String, horasBrutas: Double, horasPonderadas: Double)
      var sesiones = List[Sesion]()

      val rsT = conn.createStatement().executeQuery("SELECT fecha, tipo FROM trainings ORDER BY fecha ASC")
      while (rsT.next()) {
        val tipo = Option(rsT.getString("tipo")).getOrElse("")
        val peso = pesoTipoPractica(tipo)
        sesiones = sesiones :+ Sesion(rsT.getDate("fecha").toString, tipo, horasPorSesionTrainingDefault, horasPorSesionTrainingDefault * peso)
      }

      val rsM = conn.createStatement().executeQuery("SELECT fecha, minutos FROM matches WHERE status = 'PLAYED' AND minutos > 0")
      while (rsM.next()) {
        val horasPartido = rsM.getInt("minutos") / 60.0
        sesiones = sesiones :+ Sesion(rsM.getDate("fecha").toString, "Partido", horasPartido, horasPartido * pesoTipoPractica("Partido"))
      }

      if (sesiones.isEmpty)
        return Map("totalHoras" -> 0.0, "desglose" -> List.empty[Map[String, Any]], "porMes" -> List.empty[Map[String, Any]],
          "categorias" -> List.empty[String], "ritmoMensual" -> 0.0, "proyeccionAnio1000" -> "—")

      val totalHoras = sesiones.map(_.horasPonderadas).sum

      val desglose = sesiones.groupBy(s => categoriaPractica(s.tipo)).map { case (cat, ls) =>
        Map[String, Any]("categoria" -> cat, "horasBrutas" -> ls.map(_.horasBrutas).sum, "horasPonderadas" -> ls.map(_.horasPonderadas).sum)
      }.toList.sortBy(m => -m("horasPonderadas").asInstanceOf[Double])

      val categorias = desglose.map(_("categoria").asInstanceOf[String])

      val hoy = LocalDate.now()
      val meses12 = (0 to 11).map(i => hoy.minusMonths(11 - i)).map(d => f"${d.getYear}-${d.getMonthValue}%02d").toList
      val porMes = meses12.map { mes =>
        val sesionesMes = sesiones.filter(_.fecha.take(7) == mes)
        val porCategoria = categorias.map(cat => cat -> sesionesMes.filter(s => categoriaPractica(s.tipo) == cat).map(_.horasPonderadas).sum).toMap
        Map[String, Any]("mes" -> mes, "porCategoria" -> porCategoria)
      }

      val fechaInicio = sesiones.map(_.fecha).min
      val mesesTranscurridos = math.max(1L, java.time.temporal.ChronoUnit.MONTHS.between(LocalDate.parse(fechaInicio), hoy) + 1)
      val ritmoMensual = totalHoras / mesesTranscurridos
      val proyeccionAnio1000 = if (ritmoMensual > 0 && totalHoras < 1000) {
        val mesesHasta1000 = ((1000.0 - totalHoras) / ritmoMensual).toLong
        hoy.plusMonths(mesesHasta1000).getYear.toString
      } else if (totalHoras >= 1000) "Ya alcanzado" else "—"

      Map(
        "totalHoras"          -> totalHoras,
        "desglose"            -> desglose,
        "porMes"              -> porMes,
        "categorias"          -> categorias,
        "ritmoMensual"        -> ritmoMensual,
        "proyeccionAnio1000"  -> proyeccionAnio1000
      )
    } finally { conn.close() }
  }

  // ═════════════════════════════════════════════════════════════════════════════
  // BLOQUE B — INDICE DE CONSISTENCIA (Volatility Index) — SQL puro, sin Gemini
  // ═════════════════════════════════════════════════════════════════════════════
  /** Desviacion tipica de las notas. nota = 0 es el DEFAULT de matches (sin nota), no una nota real. */
  def getVolatilityIndex(seasonId: Int = 0): Map[String, Any] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(s"""
        SELECT AVG(nota) as media, STDDEV(nota) as desviacion,
          COUNT(*) as partidos, MIN(nota) as minima, MAX(nota) as maxima
        FROM matches
        WHERE status = 'PLAYED' AND nota IS NOT NULL AND nota > 0 ${seasonFilter(seasonId)}""")
      rs.next()
      val partidos = rs.getInt("partidos")
      val desviacion = rs.getDouble("desviacion")
      val (nivel, emoji, etiqueta) =
        if (desviacion < 0.8) ("MUY_CONSISTENTE", "🟢", "Muy consistente")
        else if (desviacion <= 1.2) ("CONSISTENTE", "🟡", "Consistente")
        else if (desviacion <= 1.8) ("IRREGULAR", "🟠", "Irregular")
        else ("MUY_IRREGULAR", "🔴", "Muy irregular")
      Map(
        "suficiente" -> (partidos >= 8),
        "partidos"   -> partidos,
        "media"      -> rs.getDouble("media"),
        "desviacion" -> desviacion,
        "minima"     -> rs.getDouble("minima"),
        "maxima"     -> rs.getDouble("maxima"),
        "nivel"      -> nivel,
        "emoji"      -> emoji,
        "etiqueta"   -> etiqueta
      )
    } finally { conn.close() }
  }

  // ═════════════════════════════════════════════════════════════════════════════
  // BLOQUE C — RELLENO RETROACTIVO DEL CLIMA (Open-Meteo) — sin Gemini
  // ═════════════════════════════════════════════════════════════════════════════
  /** Recorre los partidos jugados sin clima y lo rellena. Un fallo en un partido no detiene el resto. */
  def rellenarClimaHistorico(): Map[String, Any] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT id, fecha FROM matches WHERE status = 'PLAYED' AND fecha IS NOT NULL AND fecha <= CURRENT_DATE AND (clima IS NULL OR clima = '') ORDER BY fecha")
      val pendientes = Iterator.continually(rs).takeWhile(_.next()).map(r => (r.getInt("id"), r.getDate("fecha").toString)).toList
      val up = conn.prepareStatement("UPDATE matches SET clima = ? WHERE id = ?")
      var actualizados = 0
      pendientes.foreach { case (id, fecha) =>
        val clima = getClimaParaFecha(fecha)
        if (clima.nonEmpty) {
          up.setString(1, clima); up.setInt(2, id); up.executeUpdate()
          actualizados += 1
        }
        Thread.sleep(200) // no saturar la API gratuita de Open-Meteo
      }
      val res = Map[String, Any]("actualizados" -> actualizados, "fallidos" -> (pendientes.size - actualizados), "total" -> pendientes.size)
      println(s"[CLIMA] Relleno historico: $res")
      res
    } finally { conn.close() }
  }

  // ═════════════════════════════════════════════════════════════════════════════
  // BLOQUE D — QUICK REGISTER: partido con solo rival, resultado y nota
  // ═════════════════════════════════════════════════════════════════════════════
  /** Guarda un partido jugado hoy con los datos minimos; el resto queda con los DEFAULT de la tabla. */
  def quickSaveMatch(rival: String, gf: Int, gc: Int, nota: Double): Int = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement(s"""
        INSERT INTO matches (season_id, fecha, rival, goles_favor, goles_contra, nota, status, source)
        VALUES ($temporadaActualSQL,
                CURRENT_DATE, ?, ?, ?, ?, 'PLAYED', 'quick')
        RETURNING id""")
      ps.setString(1, fixEncoding(rival)); ps.setInt(2, gf); ps.setInt(3, gc); ps.setDouble(4, nota)
      val rs = ps.executeQuery()
      if (rs.next()) rs.getInt("id") else -1
    } finally { conn.close() }
  }

  // BLOQUE S: importacion de partidos historicos (fecha,rival,goles_favor,goles_contra,nota).
  // A diferencia de importMatchesCSV, respeta la fecha de cada fila y no toca la carta FUT.
  def importarPartidosHistoricos(csv: String): Map[String, Any] = {
    val lineas = csv.split("\r?\n").map(_.trim).filter(_.nonEmpty).toList
    val datos = if (lineas.headOption.exists(_.toLowerCase.contains("rival"))) lineas.drop(1) else lineas
    val detalle = scala.collection.mutable.ListBuffer[Map[String, Any]]()
    var importados = 0; var errores = 0
    val conn = getConnection()
    try {
      val psExiste = conn.prepareStatement(
        "SELECT COUNT(*) as c FROM matches WHERE fecha = ?::date AND LOWER(TRIM(rival)) = LOWER(TRIM(?)) AND status = 'PLAYED'")
      val psInsert = conn.prepareStatement("""
        INSERT INTO matches (season_id, fecha, rival, goles_favor, goles_contra, nota, status, source)
        VALUES ((SELECT id FROM seasons WHERE fecha_inicio <= ?::date AND (fecha_fin IS NULL OR fecha_fin >= ?::date) ORDER BY id DESC LIMIT 1),
                ?::date, ?, ?, ?, ?, 'PLAYED', 'importado')
        RETURNING id""")
      datos.zipWithIndex.foreach { case (linea, i) =>
        val nLinea = i + 1
        def error(motivo: String): Unit = { errores += 1; detalle += Map("linea" -> nLinea, "ok" -> false, "texto" -> linea, "motivo" -> motivo) }
        val sep = if (!linea.contains(",") && linea.contains(";")) ";" else ","
        val p = linea.split(sep, -1).map(_.trim)
        if (p.length < 5) error("Faltan columnas (fecha,rival,goles_favor,goles_contra,nota)")
        else {
          val fecha = scala.util.Try(LocalDate.parse(p(0))).toOption
          val rival = fixEncoding(p(1))
          val gf = p(2).toIntOption.filter(_ >= 0)
          val gc = p(3).toIntOption.filter(_ >= 0)
          val nota = p(4).replace(",", ".").toDoubleOption.filter(n => n >= 0 && n <= 10)
          if (fecha.isEmpty) error("Fecha no válida (usa AAAA-MM-DD)")
          else if (fecha.get.isAfter(LocalDate.now())) error("La fecha es futura")
          else if (rival.isEmpty) error("Rival vacío")
          else if (gf.isEmpty || gc.isEmpty) error("Goles no válidos")
          else if (nota.isEmpty) error("La nota debe estar entre 0 y 10")
          else {
            psExiste.setString(1, fecha.get.toString); psExiste.setString(2, rival)
            val rsE = psExiste.executeQuery()
            if (rsE.next() && rsE.getInt("c") > 0) error("Ya existe un partido con esa fecha y rival")
            else {
              psInsert.setString(1, fecha.get.toString); psInsert.setString(2, fecha.get.toString); psInsert.setString(3, fecha.get.toString)
              psInsert.setString(4, rival); psInsert.setInt(5, gf.get); psInsert.setInt(6, gc.get); psInsert.setDouble(7, nota.get)
              psInsert.executeQuery()
              importados += 1
              detalle += Map("linea" -> nLinea, "ok" -> true, "texto" -> s"${fecha.get} · $rival ${gf.get}-${gc.get} · nota ${nota.get}", "motivo" -> "")
            }
          }
        }
      }
    } finally { conn.close() }
    if (importados > 0) new Thread(() => detectarHitos()).start()
    Map("importados" -> importados, "errores" -> errores, "detalle" -> detalle.toList)
  }

  /** Badges de origen para el historial: QUICK_PENDIENTE (registro minimo sin rubrica completa) e IMPORTADO. */
  def getMatchSourceBadges(seasonId: Int = 0): Map[Int, String] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(s"""
        SELECT id, CASE WHEN source = 'importado' THEN 'IMPORTADO' ELSE 'QUICK_PENDIENTE' END as badge
        FROM matches
        WHERE status = 'PLAYED' ${seasonFilter(seasonId)}
          AND (source = 'importado'
               OR (source = 'quick' AND (rubrica_posicion IS NULL OR rubrica_decisiones IS NULL OR rubrica_pies IS NULL
                                         OR rubrica_comunicacion IS NULL OR rubrica_actitud IS NULL)))""")
      Iterator.continually(rs).takeWhile(_.next()).map(r => r.getInt("id") -> r.getString("badge")).toMap
    } finally { conn.close() }
  }

  /** Actualiza solo los 5 campos de rubrica (1-5). Usado por la edicion de partido y por el bot de Telegram. */
  def updateRubricaMatch(matchId: Int, posicion: Int, decisiones: Int, pies: Int, comunicacion: Int, actitud: Int): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("""
        UPDATE matches SET rubrica_posicion = ?, rubrica_decisiones = ?, rubrica_pies = ?,
          rubrica_comunicacion = ?, rubrica_actitud = ? WHERE id = ?""")
      Seq(posicion, decisiones, pies, comunicacion, actitud).zipWithIndex.foreach { case (v, i) => ps.setInt(i + 1, v) }
      ps.setInt(6, matchId)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  // ═════════════════════════════════════════════════════════════════════════════
  // BLOQUE H — SHAREABLE MATCH CARD: solo datos publicos (sin rubrica, analisis ni datos medicos)
  // ═════════════════════════════════════════════════════════════════════════════
  def getMatchCardData(matchId: Int): Option[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("""
        SELECT m.rival, m.goles_favor, m.goles_contra, m.nota, m.fecha, m.tipo_partido, m.torneo_nombre,
               COALESCE(s.nombre, s.categoria, '') as temporada
        FROM matches m LEFT JOIN seasons s ON s.id = m.season_id
        WHERE m.id = ? AND m.status = 'PLAYED'""")
      ps.setInt(1, matchId)
      val rs = ps.executeQuery()
      if (!rs.next()) None
      else {
        def optInt(c: String) = Option(rs.getObject(c)).map(_ => rs.getInt(c))
        Some(Map(
          "rival" -> fixEncoding(Option(rs.getString("rival")).getOrElse("")),
          "golesFavor" -> optInt("goles_favor"), "golesContra" -> optInt("goles_contra"),
          "nota" -> rs.getDouble("nota"),
          "fecha" -> Option(rs.getDate("fecha")).map(_.toString).getOrElse(""),
          "tipoPartido" -> Option(rs.getString("tipo_partido")).getOrElse(""),
          "torneoNombre" -> fixEncoding(Option(rs.getString("torneo_nombre")).getOrElse("")),
          "temporada" -> fixEncoding(rs.getString("temporada"))))
      }
    } finally { conn.close() }
  }

  // ═════════════════════════════════════════════════════════════════════════════
  // BLOQUE I — EXPLORADOR DE CORRELACIONES — Pearson con CORR() de PostgreSQL, sin Gemini
  // ═════════════════════════════════════════════════════════════════════════════
  /** Variable -> (etiqueta, subconsulta que devuelve una fila por fecha con columnas d y v). */
  val variablesCorrelacion: Seq[(String, String, String)] = Seq(
    ("nota", "⭐ Nota del partido",
      "SELECT fecha::date as d, AVG(nota) as v FROM matches WHERE status='PLAYED' AND nota > 0 GROUP BY fecha::date"),
    ("sueno_profundo", "😴 Sueño profundo (min)",
      "SELECT fecha as d, AVG(sueno_profundo_min)::float as v FROM wellness WHERE sueno_profundo_min > 0 GROUP BY fecha"),
    ("energia", "⚡ Energía (1-5)",
      "SELECT fecha as d, AVG(energia)::float as v FROM wellness WHERE energia IS NOT NULL GROUP BY fecha"),
    ("animo", "🙂 Ánimo (1-5)",
      "SELECT fecha as d, AVG(animo)::float as v FROM wellness WHERE animo IS NOT NULL GROUP BY fecha"),
    ("fc_reposo", "❤️ FC en reposo",
      "SELECT fecha as d, AVG(fc_reposo)::float as v FROM wellness WHERE fc_reposo IS NOT NULL GROUP BY fecha"),
    // ACWR del dia (aguda 7d / cronica 28d) con la misma formula de carga que getWorkloads
    ("acwr", "📈 ACWR",
      """WITH cargas AS (
           SELECT fecha::date as f, minutos * 4.0 as l FROM matches WHERE status='PLAYED' AND fecha IS NOT NULL
           UNION ALL
           SELECT fecha::date, 60.0 * COALESCE(rpe, 0) * (1 + COALESCE(fb_distancia, 0) * 0.05) FROM trainings WHERE fecha IS NOT NULL
         ), dias AS (SELECT DISTINCT f FROM cargas)
         SELECT dias.f as d,
           ((SELECT SUM(c.l) FROM cargas c WHERE c.f BETWEEN (dias.f - INTERVAL '6 days')::date AND dias.f) / 7.0) /
           NULLIF((SELECT SUM(c.l) FROM cargas c WHERE c.f BETWEEN (dias.f - INTERVAL '27 days')::date AND dias.f) / 28.0, 0) as v
         FROM dias WHERE dias.f >= (SELECT MIN(f) FROM cargas) + 21"""),
    ("paradas", "🧤 Paradas",
      "SELECT fecha::date as d, AVG(paradas)::float as v FROM matches WHERE status='PLAYED' GROUP BY fecha::date"),
    ("cpi", "🎯 CPI",
      "SELECT fecha::date as d, AVG(cpi) as v FROM matches WHERE status='PLAYED' AND cpi IS NOT NULL GROUP BY fecha::date"),
    ("scanning_rate", "👁️ Scanning rate",
      "SELECT fecha::date as d, AVG(scanning_rate)::float as v FROM matches WHERE status='PLAYED' AND scanning_rate > 0 GROUP BY fecha::date"),
    ("temperatura", "🌡️ Temperatura",
      "SELECT fecha::date as d, AVG(temperatura)::float as v FROM matches WHERE status='PLAYED' AND temperatura IS NOT NULL GROUP BY fecha::date")
  )

  def getCorrelacionPersonalizada(varX: String, varY: String, seasonId: Int = 0): Map[String, Any] = {
    val qx = variablesCorrelacion.find(_._1 == varX).map(_._3)
    val qy = variablesCorrelacion.find(_._1 == varY).map(_._3)
    if (qx.isEmpty || qy.isEmpty || varX == varY) return Map("valido" -> false, "puntos" -> 0)
    val conn = getConnection()
    try {
      // Temporada: se filtra por su rango de fechas para que aplique igual a partidos y a wellness
      val filtroTemporada = if (seasonId > 0)
        """AND x.d >= COALESCE((SELECT fecha_inicio FROM seasons WHERE id = ?), DATE '1900-01-01')
           AND x.d <= COALESCE((SELECT fecha_fin FROM seasons WHERE id = ?), CURRENT_DATE)""" else ""
      def bindTemporada(ps: java.sql.PreparedStatement): Unit =
        if (seasonId > 0) { ps.setInt(1, seasonId); ps.setInt(2, seasonId) }
      val base = s"FROM (${qx.get}) x JOIN (${qy.get}) y ON y.d = x.d WHERE x.v IS NOT NULL AND y.v IS NOT NULL $filtroTemporada"

      val psAgg = conn.prepareStatement(s"SELECT CORR(x.v, y.v) as correlacion, COUNT(*) as puntos, REGR_SLOPE(y.v, x.v) as pendiente, REGR_INTERCEPT(y.v, x.v) as ordenada $base")
      bindTemporada(psAgg)
      val rs = psAgg.executeQuery(); rs.next()
      val puntos = rs.getInt("puntos")
      def optD(c: String) = Option(rs.getObject(c)).map(_ => rs.getDouble(c))
      val corr = optD("correlacion"); val pendiente = optD("pendiente"); val ordenada = optD("ordenada")

      val psPts = conn.prepareStatement(s"SELECT x.d, x.v as xv, y.v as yv $base ORDER BY x.d")
      bindTemporada(psPts)
      val rp = psPts.executeQuery()
      val pares = Iterator.continually(rp).takeWhile(_.next()).map(r => (r.getDouble("xv"), r.getDouble("yv"), r.getDate("d").toString)).toList

      val interpretacion = corr match {
        case Some(r) =>
          val a = math.abs(r)
          val fuerza = if (a >= 0.7) "fuerte" else if (a >= 0.4) "moderada" else if (a >= 0.2) "débil" else "prácticamente nula"
          val sentido = if (a < 0.2) "" else if (r > 0) " positiva (cuando una sube, la otra tiende a subir)" else " negativa (cuando una sube, la otra tiende a bajar)"
          s"Relación $fuerza$sentido."
        case None => "Sin variación suficiente para calcular la correlación."
      }
      Map("valido" -> true, "correlacion" -> corr, "puntos" -> puntos, "pendiente" -> pendiente, "ordenada" -> ordenada,
        "pares" -> pares, "interpretacion" -> interpretacion, "suficiente" -> (puntos >= 10 && corr.isDefined))
    } finally { conn.close() }
  }

  // ═════════════════════════════════════════════════════════════════════════════
  // BLOQUE J — UNIFICACION DE NOMBRES DE RIVALES (encoding / espacios / mayusculas)
  // ═════════════════════════════════════════════════════════════════════════════
  /**
   * Corrige el encoding de cada nombre de rival distinto en matches y, si el nombre corregido
   * coincide (sin distinguir mayusculas) con otro ya existente, reasigna todos los partidos al
   * nombre canonico (el mas usado). En `rivals` (nombre UNIQUE) solo se renombra si no choca.
   */
  def unificarNombresRivales(): Map[String, Any] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT rival, COUNT(*) as n FROM matches WHERE rival IS NOT NULL AND rival <> '' GROUP BY rival")
      val usos = Iterator.continually(rs).takeWhile(_.next()).map(r => r.getString("rival") -> r.getInt("n")).toList
      // Nombre canonico por clave normalizada: el mas usado entre las variantes ya correctas
      def clave(s: String) = fixEncoding(s).trim.replaceAll("\\s+", " ").toLowerCase
      val canonico: Map[String, String] = usos.groupBy(u => clave(u._1)).map { case (k, variantes) =>
        val limpias = variantes.map { case (nombre, n) => (fixEncoding(nombre).trim.replaceAll("\\s+", " "), n) }
        val porNombre = limpias.groupBy(_._1).map { case (nom, l) => nom -> l.map(_._2).sum }
        k -> porNombre.maxBy(_._2)._1
      }
      val ps = conn.prepareStatement("UPDATE matches SET rival = ? WHERE rival = ?")
      var nombresCorregidos = 0; var partidosActualizados = 0
      usos.foreach { case (nombre, _) =>
        val destino = canonico(clave(nombre))
        if (destino != nombre) {
          ps.setString(1, destino); ps.setString(2, nombre)
          partidosActualizados += ps.executeUpdate(); nombresCorregidos += 1
        }
      }
      // rivals: renombrar solo si el nombre corregido no existe ya (evita violar UNIQUE)
      val rsR = conn.createStatement().executeQuery("SELECT nombre FROM rivals WHERE nombre IS NOT NULL")
      val nombresRivals = Iterator.continually(rsR).takeWhile(_.next()).map(_.getString("nombre")).toSet
      val psR = conn.prepareStatement("UPDATE rivals SET nombre = ? WHERE nombre = ?")
      var fichasRenombradas = 0; var fichasDuplicadas = 0
      nombresRivals.foreach { nombre =>
        val destino = canonico.getOrElse(clave(nombre), fixEncoding(nombre).trim)
        if (destino != nombre) {
          if (nombresRivals.contains(destino)) fichasDuplicadas += 1
          else { psR.setString(1, destino); psR.setString(2, nombre); fichasRenombradas += psR.executeUpdate() }
        }
      }
      Map("nombresCorregidos" -> nombresCorregidos, "partidosActualizados" -> partidosActualizados,
        "fichasRenombradas" -> fichasRenombradas, "fichasDuplicadas" -> fichasDuplicadas)
    } finally { conn.close() }
  }

  /** Nombres de rival para el autocompletado, del mas al menos usado, con el encoding corregido. */
  def getRivalesFrecuentes(limit: Int = 200): List[String] = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement(
        "SELECT rival, COUNT(*) as n FROM matches WHERE rival IS NOT NULL AND TRIM(rival) <> '' GROUP BY rival ORDER BY n DESC, rival LIMIT ?")
      ps.setInt(1, limit)
      val rs = ps.executeQuery()
      Iterator.continually(rs).takeWhile(_.next()).map(r => fixEncoding(r.getString("rival")).trim).toList.distinct
    } finally { conn.close() }
  }

  // ═════════════════════════════════════════════════════════════════════════════
  // PROTOCOLO DE RECUPERACION (Gemini en tarea programada o al pulsar el boton; nunca en el render)
  // Cache en ai_cache con clave recuperacion_<temporada>_<semana ISO>; se regenera si el ACWR cambia > 0.2.
  // ═════════════════════════════════════════════════════════════════════════════
  private def claveRecuperacion(): String = {
    val semana = LocalDate.now().format(java.time.format.DateTimeFormatter.ofPattern("YYYY-ww", java.util.Locale.forLanguageTag("es-ES")))
    s"recuperacion_${getTemporadaActivaId()}_$semana"
  }

  /** Protocolo guardado esta semana: (texto, acwr con el que se genero, fecha). */
  def getProtocoloRecuperacion(): Option[(String, Double, String)] = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("SELECT respuesta, creado_en FROM ai_cache WHERE prompt_hash = ?")
      ps.setString(1, claveRecuperacion())
      val rs = ps.executeQuery()
      if (!rs.next()) None
      else scala.util.Try(ujson.read(rs.getString("respuesta"))).toOption
        .map(j => (j("texto").str, j("acwr").num, Option(rs.getTimestamp("creado_en")).map(_.toString.take(16)).getOrElse("")))
    } finally { conn.close() }
  }

  /** ACWR alto (umbral de riesgo de su edad) o riesgo de lesion ALTO/CRITICO. */
  def necesitaProtocoloRecuperacion(): Boolean = {
    val e = calcularACWRConEstado()
    val acwrAlto = e("status") != "INSUFICIENTE" && e("acwr").asInstanceOf[Double] > umbralesACWR().riesgo
    acwrAlto || calcularRiesgoLesion()("riesgo").asInstanceOf[Double] >= 4.0
  }

  /** Genera (o devuelve el de esta semana si el ACWR no ha cambiado > 0.2). Left = error de la IA. */
  def generarProtocoloRecuperacion(forzar: Boolean = false): Either[String, String] = {
    val estado = calcularACWRConEstado()
    val acwr = estado("acwr").asInstanceOf[Double]
    getProtocoloRecuperacion() match {
      case Some((texto, acwrGuardado, _)) if !forzar && math.abs(acwr - acwrGuardado) <= 0.2 => return Right(texto)
      case _ =>
    }
    val card = getLatestCardData()
    val edad = calcularEdadExacta(card.fechaNacimiento)
    val peso = {
      val conn = getConnection()
      try {
        val rs = conn.createStatement().executeQuery("SELECT peso FROM physical_growth WHERE peso > 0 ORDER BY fecha DESC, id DESC LIMIT 1")
        if (rs.next()) f"${rs.getDouble("peso")}%.1f" else "desconocido"
      } finally { conn.close() }
    }
    val fasePhv = try getBioBandingData().getOrElse("faseBio", "").toString match { case "" => "desconocida"; case f => f } catch { case _: Exception => "desconocida" }
    val riesgo = calcularRiesgoLesion()
    val dias = Seq("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")
    val estructura = getWeeklyStructure().filter(_("activo").asInstanceOf[Boolean])
      .groupBy(_("diaSemana").asInstanceOf[Int]).toSeq.sortBy(_._1)
      .map { case (d, l) => s"${dias(d - 1)}: ${l.map(_("tipoSesion")).mkString(" + ")}" }.mkString("; ")
    val acwrTxt = if (estado("status") == "INSUFICIENTE") "sin histórico suficiente" else f"$acwr%.2f"
    val prompt = s"""Eres un preparador físico especializado en fútbol base pediátrico. Héctor tiene $edad años, pesa ${peso}kg, está en fase $fasePhv de maduración. Su ACWR actual es $acwrTxt y su riesgo de lesión es ${riesgo("clasificacion")} (${f"${riesgo("riesgo").asInstanceOf[Double]}%.1f"}/10; factores: ${riesgo("factoresActivos").asInstanceOf[List[String]].mkString(", ")}). Los entrenamientos previstos esta semana son: $estructura. Genera un protocolo de recuperación activa concreto y específico para esta semana, día a día, que le permita llegar al partido del sábado en las mejores condiciones posibles. Incluye: qué hacer en cada sesión de entrenamiento (intensidad reducida, tipo de trabajo, duración máxima), qué hacer en casa (sueño, hidratación, estiramientos específicos), y qué señales de alarma vigilar. Máximo 6 líneas en total — una por día de la semana. Tono práctico y directo para un padre, no clínico. Texto plano, una línea por día empezando por el nombre del día."""
    val texto = AIProvider.ask(prompt, None, bypassCache = true).replace("```", "").trim
    if (texto.isEmpty || texto.startsWith("Error")) return Left(if (texto.isEmpty) "Respuesta vacía de la IA" else texto)
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement(
        "INSERT INTO ai_cache (prompt_hash, respuesta, creado_en) VALUES (?, ?, NOW()) ON CONFLICT (prompt_hash) DO UPDATE SET respuesta = EXCLUDED.respuesta, creado_en = NOW()")
      ps.setString(1, claveRecuperacion()); ps.setString(2, ujson.write(ujson.Obj("texto" -> texto, "acwr" -> acwr)))
      ps.executeUpdate()
    } finally { conn.close() }
    Right(texto)
  }

  /** Tarea programada: si hace falta, asegura el protocolo de la semana (solo llama a Gemini si falta o cambio el ACWR). */
  def comprobarProtocoloRecuperacion(): Unit =
    if (necesitaProtocoloRecuperacion()) generarProtocoloRecuperacion() match {
      case Left(e) => println(s"[Recuperacion] ${e.take(200)}")
      case Right(_) =>
    }

  // ═════════════════════════════════════════════════════════════════════════════
  // NUTRICION E HIDRATACION PRE-PARTIDO. SQL puro, sin Gemini
  // ═════════════════════════════════════════════════════════════════════════════
  def guardarNutricionPrepartido(matchId: Int, horasUltimaComida: Option[Int], hidratacion: Option[String], desayuno: Option[Boolean]): Unit = {
    if (matchId <= 0 || (horasUltimaComida.isEmpty && hidratacion.isEmpty && desayuno.isEmpty)) return
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("UPDATE matches SET horas_ultima_comida = ?, hidratacion_prepartido = ?, desayuno_completo = ? WHERE id = ?")
      horasUltimaComida match { case Some(h) => ps.setInt(1, h); case None => ps.setNull(1, java.sql.Types.INTEGER) }
      hidratacion match { case Some(h) => ps.setString(2, h); case None => ps.setNull(2, java.sql.Types.VARCHAR) }
      desayuno match { case Some(d) => ps.setBoolean(3, d); case None => ps.setNull(3, java.sql.Types.BOOLEAN) }
      ps.setInt(4, matchId)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  val etiquetasHidratacion: Map[String, String] = Map("BIEN" -> "💧 Bien hidratado", "NORMAL" -> "🫗 Normal", "POCO" -> "😰 Poco hidratado")

  /** Nota media por nivel de hidratacion (y por desayuno). suficiente: >=10 partidos con datos de nutricion. */
  def getNutricionAnalysis(seasonId: Int = 0): Map[String, Any] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(s"""
        SELECT hidratacion_prepartido, AVG(nota) as nota_media, COUNT(*) as partidos
        FROM matches
        WHERE status = 'PLAYED' AND nota > 0 AND hidratacion_prepartido IS NOT NULL
          ${seasonFilter(seasonId)}
        GROUP BY hidratacion_prepartido""")
      val grupos = Iterator.continually(rs).takeWhile(_.next())
        .map(r => r.getString("hidratacion_prepartido") -> (r.getDouble("nota_media"), r.getInt("partidos"))).toMap
      val rsD = conn.createStatement().executeQuery(s"""
        SELECT desayuno_completo, AVG(nota) as nota_media, COUNT(*) as partidos FROM matches
        WHERE status = 'PLAYED' AND nota > 0 AND desayuno_completo IS NOT NULL ${seasonFilter(seasonId)}
        GROUP BY desayuno_completo""")
      val desayuno = Iterator.continually(rsD).takeWhile(_.next())
        .map(r => r.getBoolean("desayuno_completo") -> (r.getDouble("nota_media"), r.getInt("partidos"))).toMap
      val rsN = conn.createStatement().executeQuery(s"""
        SELECT COUNT(*) FROM matches WHERE status = 'PLAYED' ${seasonFilter(seasonId)}
          AND (hidratacion_prepartido IS NOT NULL OR horas_ultima_comida IS NOT NULL OR desayuno_completo IS NOT NULL)""")
      rsN.next()
      val n = rsN.getInt(1)
      val diferencia = for { b <- grupos.get("BIEN"); p <- grupos.get("POCO") } yield b._1 - p._1
      Map("partidos" -> n, "suficiente" -> (n >= 10), "hidratacion" -> grupos, "desayuno" -> desayuno,
        "diferenciaHidratacion" -> diferencia, "afecta" -> diferencia.exists(_ > 0.7))
    } finally { conn.close() }
  }

  /** Dia de partido: recordatorio si los 3 ultimos partidos con nota baja coincidieron con poca hidratacion. */
  def recordatorioHidratacion(): Option[String] = {
    val conn = getConnection()
    try {
      // nota baja = por debajo de la media de la temporada actual
      val rs = conn.createStatement().executeQuery(s"""
        SELECT hidratacion_prepartido FROM matches
        WHERE status = 'PLAYED' AND nota > 0 ${seasonFilterActual()}
          AND nota < (SELECT AVG(nota) FROM matches WHERE status = 'PLAYED' AND nota > 0 ${seasonFilterActual()})
        ORDER BY fecha DESC LIMIT 3""")
      val ultimas = Iterator.continually(rs).takeWhile(_.next()).map(r => Option(r.getString("hidratacion_prepartido"))).toList
      if (ultimas.size == 3 && ultimas.forall(_.contains("POCO")))
        Some("💧 Recuerda que Héctor rinde mejor bien hidratado — asegúrate de que beba suficiente esta mañana.")
      else None
    } finally { conn.close() }
  }

  // ═════════════════════════════════════════════════════════════════════════════
  // BLOQUE J — CALIBRACION DEL PADRE COMO OBSERVADOR (Gemini solo al pulsar los botones de /admin)
  // ═════════════════════════════════════════════════════════════════════════════
  /** Genera 3 situaciones (con puntuacion de referencia oculta) y crea una calibracion pendiente. Left = error. */
  def generarCalibracion(): Either[String, Int] = {
    val claves = dimensionesRubrica.map(_._1).mkString(", ")
    val edad = calcularEdadExacta(getLatestCardData().fechaNacimiento)
    // el mes en el prompt hace que ai_cache devuelva situaciones nuevas cada mes, no siempre las mismas
    val prompt = s"""Eres entrenador de porteros de fútbol base. Escribe 3 situaciones breves y concretas de un partido de un portero de $edad años (Fútbol 7), cada una de 1-2 frases, SIN valorar ni insinuar si lo hizo bien o mal. Cada situación debe evaluar una dimensión distinta de estas: $claves. Para cada una indica la puntuación de referencia de 1 a 5 que le daría un entrenador experto y objetivo (1=muy mal, 3=correcto, 5=excelente), con variedad entre situaciones. Referencia temporal: ${LocalDate.now().toString.take(7)}.
Devuelve ÚNICAMENTE un JSON válido sin backticks: [{"situacion": "...", "dimension": "<una de: $claves>", "referencia": N}, ...]"""
    val resp = AIProvider.ask(prompt)
    if (resp.startsWith("Error")) return Left(resp)
    val situaciones = try {
      ujson.read(resp.replace("```json", "").replace("```", "").trim).arr.toList.flatMap { j =>
        val dim = j("dimension").str.trim.toLowerCase
        val ref = j("referencia").num.toInt
        if (dimensionesRubrica.exists(_._1 == dim) && ref >= 1 && ref <= 5)
          Some(ujson.Obj("situacion" -> j("situacion").str.trim, "dimension" -> dim, "referencia" -> ref)) else None
      }
    } catch { case _: Exception => Nil }
    if (situaciones.size < 3) return Left("La IA no devolvió 3 situaciones válidas. Vuelve a intentarlo.")
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("INSERT INTO calibracion_padre (situaciones) VALUES (?) RETURNING id")
      ps.setString(1, ujson.write(ujson.Arr(situaciones.take(3): _*)))
      val rs = ps.executeQuery(); rs.next(); Right(rs.getInt("id"))
    } finally { conn.close() }
  }

  /** Calibracion pendiente (generada y aun sin puntuar), con las situaciones SIN la referencia. */
  def getCalibracionPendiente(): Option[(Int, List[(String, String)])] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT id, situaciones FROM calibracion_padre WHERE puntuaciones IS NULL ORDER BY id DESC LIMIT 1")
      if (!rs.next()) None
      else Some(rs.getInt("id") -> ujson.read(rs.getString("situaciones")).arr.toList.map(j => (j("situacion").str, j("dimension").str)))
    } finally { conn.close() }
  }

  /** Guarda las puntuaciones del padre, compara con la referencia y pide a Gemini la conclusion. */
  def guardarCalibracion(id: Int, puntuaciones: List[Int]): Either[String, String] = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("SELECT situaciones FROM calibracion_padre WHERE id = ? AND puntuaciones IS NULL")
      ps.setInt(1, id)
      val rs = ps.executeQuery()
      if (!rs.next()) return Left("Esa calibración ya no está pendiente.")
      val sits = ujson.read(rs.getString("situaciones")).arr.toList
      if (puntuaciones.size != sits.size || puntuaciones.exists(p => p < 1 || p > 5)) return Left("Puntúa las 3 situaciones del 1 al 5.")
      val filas = sits.zip(puntuaciones).map { case (j, p) => (j("situacion").str, j("dimension").str, j("referencia").num.toInt, p) }
      // desviacion por dimension (padre - referencia); la de mayor valor absoluto es la conclusion principal
      val (dimMax, desvMax) = filas.map { case (_, d, r, p) => d -> (p - r).toDouble }.maxBy(x => math.abs(x._2))
      val etiqueta = dimensionesRubrica.find(_._1 == dimMax).map(_._3).getOrElse(dimMax)
      val tabla = filas.map { case (s, d, r, p) => s"- [$d] \"$s\" → referencia $r, padre $p" }.mkString("\n")
      val prompt = s"""Un padre está calibrando cómo puntúa (1-5) a su hijo portero en la rúbrica. Estas son 3 situaciones con la puntuación de referencia de un entrenador experto y la del padre:
$tabla
En 2 frases, en segunda persona y en tono amable, dile si tiende a ser más exigente o más generoso de lo esperado y en qué tipo de situaciones, y cómo ajustar mentalmente sus valoraciones en esa dimensión cuando registre partidos. Texto plano, sin listas."""
      val ia = AIProvider.ask(prompt)
      val resultado =
        if (!ia.startsWith("Error") && ia.trim.nonEmpty) ia.trim
        else if (desvMax == 0) "Tus valoraciones coinciden con la referencia en las tres situaciones."
        else f"Tiendes a ser ${if (desvMax < 0) "más exigente" else "más generoso"} de lo esperado en ${etiqueta.toLowerCase} (${math.abs(desvMax)}%.0f puntos). Ajusta mentalmente tus valoraciones en esa dimensión cuando registres partidos."
      val up = conn.prepareStatement("UPDATE calibracion_padre SET puntuaciones = ?, resultado = ?, dimension = ?, desviacion = ? WHERE id = ?")
      up.setString(1, ujson.write(ujson.Arr(puntuaciones.map(ujson.Num(_)): _*))); up.setString(2, resultado)
      up.setString(3, dimMax); up.setDouble(4, desvMax); up.setInt(5, id)
      up.executeUpdate()
      Right(resultado)
    } finally { conn.close() }
  }

  /** Ultima calibracion completada: (fecha, resultado, dimension, desviacion). */
  def getUltimaCalibracion(): Option[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT fecha, resultado, dimension, desviacion FROM calibracion_padre WHERE puntuaciones IS NOT NULL ORDER BY id DESC LIMIT 1")
      if (!rs.next()) None
      else Some(Map("fecha" -> rs.getDate("fecha").toString, "resultado" -> rs.getString("resultado"),
        "dimension" -> rs.getString("dimension"), "desviacion" -> rs.getDouble("desviacion")))
    } finally { conn.close() }
  }

  // ═════════════════════════════════════════════════════════════════════════════
  // BLOQUE H — STREAK DE REGISTRO (SQL puro, sin Gemini)
  // ═════════════════════════════════════════════════════════════════════════════
  /**
   * streakSueno: dias consecutivos con sueno registrado hasta hoy (si hoy aun no hay registro cuenta
   * desde ayer: la racha no se rompe por la manana). mejorStreak: la racha mas larga de la historia.
   */
  def getStreakRegistro(): Map[String, Any] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(
        "SELECT DISTINCT fecha FROM wellness WHERE horas_sueno > 0 AND fecha <= CURRENT_DATE ORDER BY fecha DESC")
      val fechas = Iterator.continually(rs).takeWhile(_.next()).map(_.getDate("fecha").toLocalDate).toList
      val conjunto = fechas.toSet
      val hoy = LocalDate.now()
      val desde = if (conjunto.contains(hoy)) hoy else hoy.minusDays(1)
      val streak = Iterator.iterate(desde)(_.minusDays(1)).takeWhile(conjunto.contains).size
      // racha mas larga: recorrido cronologico
      var mejor = 0; var actual = 0; var previa: Option[LocalDate] = None
      fechas.reverse.foreach { f =>
        actual = if (previa.contains(f.minusDays(1))) actual + 1 else 1
        mejor = math.max(mejor, actual); previa = Some(f)
      }
      val semana = fechas.count(f => !f.isBefore(hoy.minusDays(6)))
      val rsR = conn.createStatement().executeQuery(s"""
        SELECT COUNT(*) as total,
          COUNT(*) FILTER (WHERE rubrica_posicion IS NOT NULL AND rubrica_decisiones IS NOT NULL AND rubrica_pies IS NOT NULL
                             AND rubrica_comunicacion IS NOT NULL AND rubrica_actitud IS NOT NULL) as con_rubrica
        FROM matches WHERE status = 'PLAYED' ${seasonFilterActual()}""")
      rsR.next()
      Map("streakSueno" -> streak, "mejorStreak" -> mejor, "diasSemana" -> semana,
        "partidos" -> rsR.getInt("total"), "partidosConRubrica" -> rsR.getInt("con_rubrica"))
    } finally { conn.close() }
  }

  /** Telegram cuando la racha actual supera el mejor record guardado (la primera vez solo se guarda). */
  def comprobarRecordStreak(): Option[String] = {
    val s = getStreakRegistro()
    val streak = s("streakSueno").asInstanceOf[Int]
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("SELECT payload FROM feature_cache WHERE cache_key = 'record_streak_sueno'")
      val guardado = if (rs.next()) rs.getString("payload").toIntOption else None
      val record = guardado.getOrElse(s("mejorStreak").asInstanceOf[Int])
      if (guardado.isEmpty || streak > record) {
        val ps = conn.prepareStatement(
          "INSERT INTO feature_cache (cache_key, payload, updated_at) VALUES ('record_streak_sueno', ?, NOW()) ON CONFLICT (cache_key) DO UPDATE SET payload = EXCLUDED.payload, updated_at = NOW()")
        ps.setString(1, math.max(streak, record).toString); ps.executeUpdate()
      }
      if (guardado.isDefined && streak > record) Some(s"🔥 Nuevo récord de streak de sueño: $streak días consecutivos.") else None
    } finally { conn.close() }
  }

  // ═════════════════════════════════════════════════════════════════════════════
  // BLOQUE G — DATA QUALITY SCORE (0-100). SQL puro, sin Gemini
  // ═════════════════════════════════════════════════════════════════════════════
  /**
   * Componentes (peso): sueno ultimos 30 dias (30%), partidos con rubrica completa (25%), continuidad de
   * carga = semanas con alguna sesion en las ultimas 12 (20%), meses con La Voz del Portero (15%),
   * sesiones de academia con feedback (10%). Un componente sin nada que medir (0 partidos, 0 sesiones
   * de academia...) se excluye y su peso se reparte entre los demas.
   */
  def getDataQualityScore(seasonId: Int = 0): Map[String, Any] = {
    val sid = if (seasonId > 0) seasonId else getTemporadaActivaId()
    val conn = getConnection()
    try {
      // Inicio de la temporada (o hace 30 dias si no hay temporada o no tiene fecha)
      val inicio: LocalDate = {
        val ps = conn.prepareStatement("SELECT fecha_inicio FROM seasons WHERE id = ?")
        ps.setInt(1, sid)
        val rs = ps.executeQuery()
        (if (rs.next()) Option(rs.getDate("fecha_inicio")).map(_.toLocalDate) else None).getOrElse(LocalDate.now().minusDays(29))
      }
      val hoy = LocalDate.now()
      def cuenta(sql: String, params: Any*): Int = {
        val ps = conn.prepareStatement(sql)
        params.zipWithIndex.foreach { case (p, i) => p match {
          case d: LocalDate => ps.setDate(i + 1, java.sql.Date.valueOf(d)); case n: Int => ps.setInt(i + 1, n); case o => ps.setString(i + 1, o.toString) } }
        val rs = ps.executeQuery(); if (rs.next()) rs.getInt(1) else 0
      }
      // 1. Sueno: ultimos 30 dias (o desde el inicio de temporada si es mas reciente)
      val desdeSueno = if (inicio.isAfter(hoy.minusDays(29))) inicio else hoy.minusDays(29)
      val diasTotales = java.time.temporal.ChronoUnit.DAYS.between(desdeSueno, hoy).toInt + 1
      val diasConSueno = cuenta("SELECT COUNT(DISTINCT fecha) FROM wellness WHERE horas_sueno > 0 AND fecha >= ? AND fecha <= ?", desdeSueno, hoy)
      // 2. Rubrica completa
      val totalPartidos = cuenta(s"SELECT COUNT(*) FROM matches WHERE status = 'PLAYED' ${seasonFilter(sid)}")
      val conRubrica = cuenta(s"""SELECT COUNT(*) FROM matches WHERE status = 'PLAYED' ${seasonFilter(sid)}
        AND rubrica_posicion IS NOT NULL AND rubrica_decisiones IS NOT NULL AND rubrica_pies IS NOT NULL
        AND rubrica_comunicacion IS NOT NULL AND rubrica_actitud IS NOT NULL""")
      // 3. Continuidad de carga: semanas (ultimas 12 de la temporada) con al menos una sesion
      val desdeSemanas = { val d = hoy.minusWeeks(11).`with`(java.time.DayOfWeek.MONDAY); if (inicio.isAfter(d)) inicio else d }
      val semanasTotales = (java.time.temporal.ChronoUnit.WEEKS.between(desdeSemanas.`with`(java.time.DayOfWeek.MONDAY), hoy) + 1).toInt
      val semanasConCarga = cuenta("""SELECT COUNT(DISTINCT TO_CHAR(f, 'IYYY-IW')) FROM (
          SELECT fecha as f FROM trainings WHERE fecha >= ? AND fecha <= ? AND tipo_ausencia IS NULL
          UNION ALL SELECT fecha FROM matches WHERE status = 'PLAYED' AND fecha >= ? AND fecha <= ?) t""", desdeSemanas, hoy, desdeSemanas, hoy)
      // 4. La Voz del Portero: meses de la temporada con registro
      val mesesTotales = (java.time.temporal.ChronoUnit.MONTHS.between(inicio.withDayOfMonth(1), hoy.withDayOfMonth(1)) + 1).toInt
      val mesesConVoz = cuenta("SELECT COUNT(DISTINCT TO_CHAR(fecha, 'YYYY-MM')) FROM voz_portero WHERE fecha >= ? AND fecha <= ?", inicio.withDayOfMonth(1), hoy)
      // 5. Feedback de academia
      val sesionesAcademia = cuenta("SELECT COUNT(*) FROM trainings WHERE LOWER(tipo) LIKE '%academia%' AND tipo_ausencia IS NULL AND fecha >= ?", inicio)
      val conFeedback = cuenta("SELECT COUNT(*) FROM trainings WHERE LOWER(tipo) LIKE '%academia%' AND tipo_ausencia IS NULL AND fecha >= ? AND feedback_entrenador IS NOT NULL AND TRIM(feedback_entrenador) <> ''", inicio)

      // (clave, etiqueta, peso, numerador, denominador, detalle)
      val componentes = List(
        ("sueno", "💤 Sueño", 0.30, diasConSueno, diasTotales, s"$diasConSueno/$diasTotales días"),
        ("rubrica", "📊 Rúbrica completa", 0.25, conRubrica, totalPartidos, s"$conRubrica/$totalPartidos partidos"),
        ("continuidad", "📈 Continuidad ACWR", 0.20, semanasConCarga, semanasTotales, s"$semanasConCarga/$semanasTotales semanas con sesiones"),
        ("voz", "🎤 Voz del Portero", 0.15, mesesConVoz, mesesTotales, s"$mesesConVoz/$mesesTotales meses"),
        ("academia", "🎓 Feedback academia", 0.10, conFeedback, sesionesAcademia, s"$conFeedback/$sesionesAcademia sesiones"))
        .map { case (k, et, peso, num, den, det) =>
          val pct: Option[Int] = if (den > 0) Some(math.min(100, num * 100 / den)) else None
          Map[String, Any]("clave" -> k, "etiqueta" -> et, "peso" -> peso, "pct" -> pct, "detalle" -> (if (den > 0) det else "sin datos que medir"))
        }
      val medibles = componentes.filter(_("pct").asInstanceOf[Option[Int]].isDefined)
      val pesoTotal = medibles.map(_("peso").asInstanceOf[Double]).sum
      val score = if (pesoTotal <= 0) 0
        else (medibles.map(c => c("pct").asInstanceOf[Option[Int]].get * c("peso").asInstanceOf[Double]).sum / pesoTotal).toInt
      val nivel = if (score >= 80) "EXCELENTE" else if (score >= 60) "BUENO" else if (score >= 40) "MEJORABLE" else "INSUFICIENTE"
      val avisos = List(
        if (totalPartidos - conRubrica > 0) Some(s"⚠️ ${totalPartidos - conRubrica} partidos sin rúbrica completa — el arquetipo tiene baja confianza hasta que se completen.") else None
      ).flatten
      val masBajo = medibles.sortBy(_("pct").asInstanceOf[Option[Int]].get).headOption.map(_("etiqueta").toString)
      Map("score" -> score, "nivel" -> nivel, "componentes" -> componentes, "avisos" -> avisos, "componenteMasBajo" -> masBajo)
    } finally { conn.close() }
  }

  /** Barra de texto para el email/Telegram: ████████░░ */
  def barraTexto(pct: Int, ancho: Int = 10): String = { val llenos = math.round(pct * ancho / 100.0).toInt; "█" * llenos + "░" * (ancho - llenos) }

  /**
   * Linea para el email del lunes si el score bajo respecto a la semana anterior. El score se guarda por
   * semana ISO, asi que generar el resumen varias veces la misma semana no altera la comparacion.
   */
  def dataQualityCambioSemanal(): Option[String] = {
    val dq = getDataQualityScore()
    val score = dq("score").asInstanceOf[Int]
    val hoy = LocalDate.now()
    val fmt = java.time.format.DateTimeFormatter.ofPattern("YYYY-ww", java.util.Locale.forLanguageTag("es-ES"))
    val claveHoy = s"dq_score_${hoy.format(fmt)}"
    val claveAnterior = s"dq_score_${hoy.minusWeeks(1).format(fmt)}"
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("SELECT payload FROM feature_cache WHERE cache_key = ?")
      ps.setString(1, claveAnterior)
      val rs = ps.executeQuery()
      val anterior = if (rs.next()) rs.getString("payload").toIntOption else None
      val up = conn.prepareStatement(
        "INSERT INTO feature_cache (cache_key, payload, updated_at) VALUES (?, ?, NOW()) ON CONFLICT (cache_key) DO UPDATE SET payload = EXCLUDED.payload, updated_at = NOW()")
      up.setString(1, claveHoy); up.setString(2, score.toString); up.executeUpdate()
      anterior.filter(_ > score).map { a =>
        s"📊 La calidad de datos bajó de $a a $score esta semana. El mayor gap: ${dq("componenteMasBajo").asInstanceOf[Option[String]].getOrElse("—")}."
      }
    } finally { conn.close() }
  }

  // ═════════════════════════════════════════════════════════════════════════════
  // BLOQUE F — FASES DE GUARDIAN (transparencia sobre cuantos datos hay). SQL puro, sin Gemini
  // ═════════════════════════════════════════════════════════════════════════════
  private val fasesGuardian: Seq[Map[String, Any]] = Seq(
    Map("numero" -> 1, "nombre" -> "CONSTRUCCIÓN", "emoji" -> "🏗️",
      "descripcion" -> "Guardian está acumulando datos. Los análisis avanzados se activarán progresivamente.",
      "modulosActivos" -> List("Sueño", "ACWR básico", "Rúbrica", "La Voz del Portero", "Índice de Forma")),
    Map("numero" -> 2, "nombre" -> "APRENDIZAJE", "emoji" -> "🌱",
      "descripcion" -> "Los primeros patrones están emergiendo. Resultados orientativos.",
      "modulosActivos" -> List("Arquetipo (orientativo)", "Z-Score (orientativo)", "Firma de fatiga (básica)")),
    Map("numero" -> 3, "nombre" -> "ANÁLISIS", "emoji" -> "📊",
      "descripcion" -> "Base de datos sólida. Los análisis son estadísticamente fiables.",
      "modulosActivos" -> List("Simulador What-if", "Correlaciones robustas", "CPI fiable", "Firma de fatiga consolidada")),
    Map("numero" -> 4, "nombre" -> "HISTÓRICO", "emoji" -> "🔬",
      "descripcion" -> "Guardian tiene suficiente historia para análisis longitudinal completo.",
      "modulosActivos" -> List("Markov Career Pathing", "Comparativa longitudinal", "Predicciones")))

  /** seasonId = 0: todos los datos (la fase es del sistema, no de una temporada). */
  def getFaseGuardian(seasonId: Int = 0): Map[String, Any] = {
    val conn = getConnection()
    val (totalPartidos, totalSemanasSueno, totalTemporadas) = try {
      val rs = conn.createStatement().executeQuery(s"""
        SELECT
          (SELECT COUNT(*) FROM matches WHERE status = 'PLAYED' ${seasonFilter(seasonId)}) as partidos,
          (SELECT COUNT(DISTINCT TO_CHAR(fecha, 'IYYY-IW')) FROM wellness WHERE horas_sueno > 0) as semanas_sueno,
          (SELECT COUNT(DISTINCT season_id) FROM matches WHERE status = 'PLAYED' AND season_id IS NOT NULL) as temporadas""")
      rs.next()
      (rs.getInt("partidos"), rs.getInt("semanas_sueno"), rs.getInt("temporadas"))
    } finally { conn.close() }
    val numero =
      if (totalPartidos < 5 || totalSemanasSueno < 3) 1
      else if (totalPartidos < 25 || totalSemanasSueno < 12) 2
      else if (totalPartidos < 60 || totalTemporadas < 2) 3
      else 4
    // Requisitos para la siguiente fase: (partidos, semanas con sueno, temporadas)
    val siguienteReq: Option[(Int, Int, Int)] = numero match {
      case 1 => Some((5, 3, 0)); case 2 => Some((25, 12, 0)); case 3 => Some((60, 0, 2)); case _ => None
    }
    val faltan: List[String] = siguienteReq.toList.flatMap { case (p, s, t) =>
      List(
        if (totalPartidos < p) Some(s"${p - totalPartidos} partidos con datos") else None,
        if (totalSemanasSueno < s) Some(s"${s - totalSemanasSueno} semanas con sueño registrado") else None,
        if (totalTemporadas < t) Some(s"${t - totalTemporadas} temporada(s) más") else None).flatten
    }
    fasesGuardian(numero - 1) ++ Map(
      "totalPartidos" -> totalPartidos, "totalSemanasSueno" -> totalSemanasSueno, "totalTemporadas" -> totalTemporadas,
      "siguiente" -> fasesGuardian.lift(numero), "faltan" -> faltan)
  }

  /** Mensaje de Telegram si la fase ha subido desde la ultima comprobacion (la primera vez solo se guarda). */
  def comprobarCambioFaseGuardian(): Option[String] = {
    val fase = getFaseGuardian()
    val numero = fase("numero").asInstanceOf[Int]
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("SELECT payload FROM feature_cache WHERE cache_key = 'fase_guardian'")
      val anterior = if (rs.next()) rs.getString("payload").toIntOption else None
      if (!anterior.contains(numero)) {
        val ps = conn.prepareStatement(
          "INSERT INTO feature_cache (cache_key, payload, updated_at) VALUES ('fase_guardian', ?, NOW()) ON CONFLICT (cache_key) DO UPDATE SET payload = EXCLUDED.payload, updated_at = NOW()")
        ps.setString(1, numero.toString); ps.executeUpdate()
      }
      anterior.filter(_ < numero).map { _ =>
        s"📊 Guardian entra en Fase $numero — ${fase("descripcion")} Nuevos módulos activos: ${fase("modulosActivos").asInstanceOf[List[String]].mkString(", ")}."
      }
    } finally { conn.close() }
  }

  // ═════════════════════════════════════════════════════════════════════════════
  // BLOQUE E — RPE FUNCIONAL DE HECTOR (SQL puro, sin Gemini)
  // ═════════════════════════════════════════════════════════════════════════════
  val etiquetasRpeHector: Seq[String] = Seq("Fresco", "Normal", "Algo cansado", "Muy cansado", "Agotado")

  /** E3: FC en reposo de la manana siguiente al entreno > media historica + 5 => el RPE registrado puede quedarse corto. */
  def validarRPEconFC(trainingId: Int): Option[String] = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("""
        SELECT t.rpe,
          (SELECT fc_reposo FROM wellness WHERE fecha = t.fecha + 1 AND fc_reposo IS NOT NULL) as fc_siguiente,
          (SELECT AVG(fc_reposo) FROM wellness WHERE fc_reposo IS NOT NULL AND fecha < t.fecha + 1 AND fecha >= t.fecha - 30) as fc_media,
          (SELECT COUNT(*) FROM wellness WHERE fc_reposo IS NOT NULL) as n_fc
        FROM trainings t WHERE t.id = ? AND t.tipo_ausencia IS NULL AND t.rpe > 0""")
      ps.setInt(1, trainingId)
      val rs = ps.executeQuery()
      if (!rs.next() || rs.getInt("n_fc") < 10) return None
      val fc = Option(rs.getObject("fc_siguiente")).map(_ => rs.getInt("fc_siguiente"))
      val media = Option(rs.getObject("fc_media")).map(_ => rs.getDouble("fc_media"))
      (fc, media) match {
        case (Some(f), Some(m)) if f > m + 5 =>
          Some(f"⚠️ La FC de esta mañana ($f BPM, +${f - m}%.0f sobre la media) sugiere que el entrenamiento de ayer fue más intenso de lo registrado (RPE=${rs.getInt("rpe")}). El ACWR puede estar subestimado.")
        case _ => None
      }
    } finally { conn.close() }
  }

  /** Avisos E3 de los entrenos de ayer (se muestran junto al registro de sueno de hoy). */
  def avisosRPEconFCHoy(): List[String] = {
    val conn = getConnection()
    val ids = try {
      val rs = conn.createStatement().executeQuery("SELECT id FROM trainings WHERE fecha = CURRENT_DATE - 1 AND tipo_ausencia IS NULL AND rpe > 0")
      Iterator.continually(rs).takeWhile(_.next()).map(_.getInt("id")).toList
    } finally { conn.close() }
    ids.flatMap(validarRPEconFC).distinct
  }

  /** E4: RPE del padre (1-10) vs RPE de Hector (1-5, se compara x2) en los entrenos con ambos datos. */
  def getDivergenciaRPE(seasonId: Int = 0): Map[String, Any] = {
    val conn = getConnection()
    try {
      val filtro = if (seasonId > 0)
        s"""AND fecha >= COALESCE((SELECT fecha_inicio FROM seasons WHERE id = $seasonId), DATE '1900-01-01')
            AND fecha <= COALESCE((SELECT fecha_fin FROM seasons WHERE id = $seasonId), CURRENT_DATE)""" else ""
      val rs = conn.createStatement().executeQuery(s"""
        SELECT COUNT(*) as n, AVG(rpe) as media_padre, AVG(rpe_hector * 2.0) as media_hector,
          SUM(CASE WHEN rpe_hector * 2 > rpe THEN 1 ELSE 0 END) as hector_mas_alto
        FROM trainings
        WHERE rpe_hector IS NOT NULL AND rpe > 0 AND tipo_ausencia IS NULL $filtro""")
      rs.next()
      val n = rs.getInt("n")
      if (n < 5) return Map("suficiente" -> false, "n" -> n)
      val mp = rs.getDouble("media_padre"); val mh = rs.getDouble("media_hector")
      // sistematico: Hector por encima de media en >= 1 punto y en la mayoria de sesiones
      val divergente = mh - mp >= 1.0 && rs.getInt("hector_mas_alto") * 2 > n
      val mensaje =
        if (divergente) f"📊 Divergencia de percepción: Héctor percibe los entrenamientos como más intensos de lo que tú registras (media padre: $mp%.1f, media Héctor: $mh%.1f). El ACWR real puede ser más alto de lo que muestra Guardian."
        else f"✅ Percepciones alineadas: media padre $mp%.1f · media Héctor $mh%.1f (escala 1-10)."
      Map("suficiente" -> true, "n" -> n, "mediaPadre" -> mp, "mediaHector" -> mh, "divergente" -> divergente, "mensaje" -> mensaje)
    } finally { conn.close() }
  }

  // ═════════════════════════════════════════════════════════════════════════════
  // BLOQUE C — INDICADORES DE CONFIANZA ESTADISTICA (sin SQL: solo el numero de observaciones)
  // ═════════════════════════════════════════════════════════════════════════════
  def getConfianzaModulo(tipo: String, n: Int): Map[String, String] = {
    val (minRojo, minAmarillo) = tipo match {
      case "arquetipo"              => (5, 25)
      case "firma_fatiga"           => (8, 20)
      case "z_score"                => (15, 30)
      case "cpi"                    => (10, 25)
      case "transferencia"          => (5, 15)
      case "correlacion"            => (10, 30)
      case "volatility_index"       => (8, 20)
      case "resilience_index"       => (10, 25)
      case "rendimiento_por_fase"   => (15, 30)
      case "1v1_angulo"             => (15, 30)
      case "paso_negativo"          => (8, 20)
      case "scanning_efectividad"   => (10, 25)
      case "rfmf_benchmarking"      => (5, 15)
      case _                        => (10, 25)
    }
    if (n < minRojo) Map(
      "nivel" -> "INSUFICIENTE",
      "emoji" -> "🔴",
      "texto" -> s"Resultado orientativo ($n observaciones). Se necesitan $minAmarillo+ para resultados robustos.",
      "color" -> "dc2626"
    ) else if (n < minAmarillo) Map(
      "nivel" -> "EMERGENTE",
      "emoji" -> "🟡",
      "texto" -> s"Patrón emergente ($n observaciones). Confirmar en próximas semanas.",
      "color" -> "ca8a04"
    ) else Map(
      "nivel" -> "ROBUSTO",
      "emoji" -> "🟢",
      "texto" -> s"Base estadística sólida ($n observaciones).",
      "color" -> "16a34a"
    )
  }

  // ═════════════════════════════════════════════════════════════════════════════
  // BLOQUE B (calidad de datos) — SESGO DE LA RUBRICA / NOTA POR RESULTADO. SQL puro, sin Gemini
  // ═════════════════════════════════════════════════════════════════════════════
  /** Correlacion resultado (victoria 1 / empate 0.5 / derrota 0) vs nota. Sesgo si r > 0.6 con >=10 partidos. */
  def calcularSesgoPorResultado(seasonId: Int = 0): Map[String, Any] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(s"""
        SELECT
          AVG(CASE WHEN goles_favor > goles_contra THEN nota END) as nota_victoria,
          AVG(CASE WHEN goles_favor < goles_contra THEN nota END) as nota_derrota,
          AVG(CASE WHEN goles_favor = goles_contra THEN nota END) as nota_empate,
          CORR(
            CASE WHEN goles_favor > goles_contra THEN 1.0
                 WHEN goles_favor = goles_contra THEN 0.5
                 ELSE 0.0 END,
            nota
          ) as correlacion_resultado_nota,
          COUNT(*) as partidos
        FROM matches
        WHERE status = 'PLAYED' AND nota IS NOT NULL AND nota > 0
          AND goles_favor IS NOT NULL AND goles_contra IS NOT NULL
          ${seasonFilter(seasonId)}""")
      rs.next()
      def opt(c: String) = Option(rs.getObject(c)).map(_ => rs.getDouble(c))
      val n = rs.getInt("partidos")
      val r = opt("correlacion_resultado_nota")
      Map("partidos" -> n, "suficiente" -> (n >= 10 && r.isDefined), "correlacion" -> r,
        "notaVictoria" -> opt("nota_victoria"), "notaDerrota" -> opt("nota_derrota"), "notaEmpate" -> opt("nota_empate"),
        "sesgo" -> (n >= 10 && r.exists(_ > 0.6)))
    } finally { conn.close() }
  }

  /**
   * Partidos con rubrica completa del padre y RUBRICA_IA en el analisis de video. Para cada uno, la
   * dimension que la IA ve mas debil y la diferencia padre - IA en esa dimension.
   */
  def getCruceRubricaVideo(seasonId: Int = 0): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery(s"""
        SELECT id, fecha, rival, video_analisis_ia, rubrica_posicion, rubrica_decisiones, rubrica_pies, rubrica_comunicacion, rubrica_actitud
        FROM matches
        WHERE status = 'PLAYED' AND video_analisis_ia IS NOT NULL AND video_analisis_ia <> ''
          AND rubrica_posicion IS NOT NULL AND rubrica_decisiones IS NOT NULL AND rubrica_pies IS NOT NULL
          AND rubrica_comunicacion IS NOT NULL AND rubrica_actitud IS NOT NULL ${seasonFilter(seasonId)}
        ORDER BY fecha DESC""")
      Iterator.continually(rs).takeWhile(_.next()).flatMap { r =>
        extractRubricaIA(r.getString("video_analisis_ia")).map { ia =>
          val (clave, columna, etiqueta) = dimensionesRubrica.minBy { case (k, _, _) => ia(k) }
          val padre = r.getInt(columna)
          Map[String, Any]("matchId" -> r.getInt("id"), "fecha" -> r.getDate("fecha").toString,
            "rival" -> fixEncoding(Option(r.getString("rival")).getOrElse("")),
            "dimension" -> clave, "etiqueta" -> etiqueta, "notaIA" -> ia(clave), "notaPadre" -> padre,
            "diferencia" -> (padre - ia(clave)).toDouble)
        }
      }.toList
    } finally { conn.close() }
  }

  /** Patron: en 3+ partidos la diferencia supera 1.5 puntos en la misma direccion. */
  def mensajeCruceRubricaVideo(cruce: List[Map[String, Any]]): Option[String] = {
    if (cruce.size < 3) return None
    val grandes = cruce.filter(c => math.abs(c("diferencia").asInstanceOf[Double]) > 1.5)
    val (generoso, critico) = grandes.partition(_("diferencia").asInstanceOf[Double] > 0)
    val grupo = if (generoso.size >= critico.size) generoso else critico
    if (grupo.size < 3) return None
    val etiqueta = grupo.groupBy(_("etiqueta").toString).maxBy(_._2.size)._1
    val media = grupo.map(c => math.abs(c("diferencia").asInstanceOf[Double])).sum / grupo.size
    val padreMasAlto = grupo.head("diferencia").asInstanceOf[Double] > 0
    Some(f"En los últimos ${cruce.size} partidos con vídeo, la IA valora ${etiqueta.toLowerCase} $media%.1f puntos " +
      s"${if (padreMasAlto) "más bajo" else "más alto"} que tú consistentemente. ¿Estás siendo " +
      s"${if (padreMasAlto) "demasiado generoso" else "demasiado crítico"} con esa dimensión?")
  }

  // ═════════════════════════════════════════════════════════════════════════════
  // BLOQUE O — DETECTOR DE ENFERMEDAD INCIPIENTE (SQL puro, sin Gemini)
  // FC en reposo > media+5 con energia y animo bajos a la vez. Requiere >=10 registros de FC.
  // ═════════════════════════════════════════════════════════════════════════════
  val mensajeEnfermedadIncipiente =
    "🤒 Posible enfermedad incipiente — la FC en reposo subió y la energía y el ánimo bajaron simultáneamente. Vigila cómo se encuentra Héctor hoy."

  def detectarEnfermedadIncipiente(): Option[String] = {
    val conn = getConnection()
    try {
      val rsN = conn.createStatement().executeQuery("SELECT COUNT(*) as n FROM wellness WHERE fc_reposo IS NOT NULL")
      if (!rsN.next() || rsN.getInt("n") < 10) return None
      // fc_hoy: la medicion mas reciente, pero solo si es de hoy o ayer (una FC antigua no dice nada de hoy)
      val rs = conn.createStatement().executeQuery("""
        SELECT
          AVG(fc_reposo) FILTER (WHERE fecha >= CURRENT_DATE - 30) as fc_media,
          (SELECT fc_reposo FROM wellness WHERE fc_reposo IS NOT NULL AND fecha >= CURRENT_DATE - 1 ORDER BY fecha DESC LIMIT 1) as fc_hoy,
          AVG(energia) FILTER (WHERE fecha >= CURRENT_DATE - 3) as energia_reciente,
          AVG(animo) FILTER (WHERE fecha >= CURRENT_DATE - 3) as animo_reciente
        FROM wellness WHERE fecha >= CURRENT_DATE - 30""")
      if (!rs.next()) return None
      def opt(c: String) = Option(rs.getObject(c)).map(_ => rs.getDouble(c))
      (opt("fc_media"), opt("fc_hoy"), opt("energia_reciente"), opt("animo_reciente")) match {
        case (Some(media), Some(hoy), Some(energia), Some(animo)) if hoy > media + 5 && energia < 3.0 && animo < 3.0 =>
          Some(mensajeEnfermedadIncipiente)
        case _ => None
      }
    } finally { conn.close() }
  }

  // ═════════════════════════════════════════════════════════════════════════════
  // BLOQUE N — TELEGRAM BOT BIDIRECCIONAL (sin Gemini: solo parseo de texto y SQL)
  // Solo tablas Elite. La sesion guarda en que paso de cada flujo esta la conversacion.
  // ═════════════════════════════════════════════════════════════════════════════
  private case class TgSesion(flujo: Option[String] = None, paso: Option[String] = None,
                              matchId: Option[Int] = None, trainingId: Option[Int] = None,
                              golesPendientes: Int = 0, golesRegistrados: Int = 0)

  /** Sesion activa del chat. Una conversacion abandonada caduca a las 6 horas. */
  private def tgSesion(chatId: String): TgSesion = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement(
        "SELECT * FROM telegram_session WHERE chat_id = ? AND updated_at > NOW() - INTERVAL '6 hours'")
      ps.setString(1, chatId)
      val rs = ps.executeQuery()
      if (!rs.next()) TgSesion()
      else {
        def optInt(c: String) = Option(rs.getObject(c)).map(_ => rs.getInt(c))
        TgSesion(Option(rs.getString("flujo")), Option(rs.getString("paso")), optInt("match_id_temp"), optInt("training_id_temp"),
          rs.getInt("goles_pendientes"), rs.getInt("goles_registrados"))
      }
    } finally { conn.close() }
  }

  private def tgGuardarSesion(chatId: String, s: TgSesion): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("""
        INSERT INTO telegram_session (chat_id, flujo, paso, match_id_temp, training_id_temp, goles_pendientes, goles_registrados, updated_at)
        VALUES (?, ?, ?, ?, ?, ?, ?, NOW())
        ON CONFLICT (chat_id) DO UPDATE SET flujo = EXCLUDED.flujo, paso = EXCLUDED.paso, match_id_temp = EXCLUDED.match_id_temp,
          training_id_temp = EXCLUDED.training_id_temp, goles_pendientes = EXCLUDED.goles_pendientes,
          goles_registrados = EXCLUDED.goles_registrados, updated_at = NOW()""")
      ps.setString(1, chatId)
      s.flujo match { case Some(v) => ps.setString(2, v); case None => ps.setNull(2, java.sql.Types.VARCHAR) }
      s.paso match { case Some(v) => ps.setString(3, v); case None => ps.setNull(3, java.sql.Types.VARCHAR) }
      s.matchId match { case Some(v) => ps.setInt(4, v); case None => ps.setNull(4, java.sql.Types.INTEGER) }
      s.trainingId match { case Some(v) => ps.setInt(5, v); case None => ps.setNull(5, java.sql.Types.INTEGER) }
      ps.setInt(6, s.golesPendientes); ps.setInt(7, s.golesRegistrados)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  private def tgLimpiarSesion(chatId: String): Unit = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("DELETE FROM telegram_session WHERE chat_id = ?")
      ps.setString(1, chatId); ps.executeUpdate()
    } finally { conn.close() }
  }

  /** UPDATE parcial de un partido. Los nombres de columna son constantes internas, nunca texto del usuario. */
  private def tgActualizarPartido(matchId: Int, campos: Seq[(String, Any)]): Unit = {
    if (campos.isEmpty) return
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement(s"UPDATE matches SET ${campos.map(c => s"${c._1} = ?").mkString(", ")} WHERE id = ?")
      campos.zipWithIndex.foreach { case ((_, v), i) =>
        v match {
          case x: Int => ps.setInt(i + 1, x)
          case x: Double => ps.setDouble(i + 1, x)
          case x: Boolean => ps.setBoolean(i + 1, x)
          case x: String => ps.setString(i + 1, x)
          case _ => ps.setNull(i + 1, java.sql.Types.NULL)
        }
      }
      ps.setInt(campos.size + 1, matchId)
      ps.executeUpdate()
    } finally { conn.close() }
  }

  private def tgNum(s: String): Option[Double] = s.replace(",", ".").toDoubleOption
  private def tgInt(s: String): Option[Int] = s.toIntOption
  private def tgEnRango(v: Option[Int], min: Int, max: Int): Option[Int] = v.filter(x => x >= min && x <= max)

  // Zonas del bot -> codigos de zona_gol que usa el formulario web (T/M/B = alto/medio/bajo, L/C/R)
  private val tgZonas: Map[String, String] = Map(
    "ALTO_IZQ" -> "TL", "ALTO_CEN" -> "TC", "ALTO_DER" -> "TR",
    "BAJO_IZQ" -> "BL", "BAJO_CEN" -> "BC", "BAJO_DER" -> "BR")
  private val tgSituaciones: Map[String, (String, String)] = Map( // codigo -> (origen, situacion) como en el formulario web
    "REMATE" -> ("Otro", "Remate"), "CABEZA" -> ("Otro", "Remate cabeza"), "FALLO" -> ("Error defensivo", "Otro"),
    "1V1" -> ("Otro", "1 vs 1"), "PENALTI" -> ("Penalti", "Penalti"), "FALTA" -> ("Falta directa", "Otro"))
  private val tgPosiciones = Set("BIEN_PLANTADO", "PASO_NEGATIVO", "DESPLAZAMIENTO_TARDIO", "IMPARABLE")
  // Minuto representativo de cada cuarto, coherente con los cortes de saveMinutoGoles (12/25/37)
  private val tgCuartos: Map[String, Int] = Map("Q1" -> 6, "Q2" -> 19, "Q3" -> 31, "Q4" -> 44)
  private val tgRegulaciones = Set("HABLA_SOLO", "RESPIRA", "ENFADO", "NEUTRAL", "REORGANIZA", "DECAIDO")
  private val tgComandos = Set("SUEÑO", "SUENO", "FC", "PARTIDO", "RUBRICA", "GOL", "PARADAS", "CONTEXTO", "EXTRAS", "1V1",
    "JUDO", "CLUB", "ACADEMIA", "PESO", "APP", "SALTAR", "LISTO", "SI", "NO", "NINGUNO", "ESTADO", "AYUDA")

  def parseTelegramMessage(texto: String, chatId: String): String = {
    try {
      val upper = texto.trim.toUpperCase
      // Pasos de texto libre (factor externo / feedback del entrenador): cualquier cosa que no sea un comando
      val primera = upper.split("\\s+").headOption.getOrElse("")
      val ses = tgSesion(chatId)
      if (!tgComandos.contains(primera) && ses.paso.contains("FACTOR")) return handleFactor(texto.trim, chatId, ses)
      if (!tgComandos.contains(primera) && ses.paso.contains("FEEDBACK")) return handleFeedback(texto.trim, chatId, ses)
      if (ses.paso.contains("RPE_HECTOR") && upper.matches("[1-5]")) return handleRpeHector(upper.toInt, chatId, ses)

      if (upper.startsWith("SUEÑO") || upper.startsWith("SUENO")) handleSueno(texto, chatId)
      else if (upper.startsWith("FC "))        handleFC(texto, chatId)
      else if (upper.startsWith("PARTIDO "))   handlePartidoStep1(texto, chatId)
      else if (upper.startsWith("RUBRICA "))   handleRubrica(texto, chatId)
      else if (upper.startsWith("GOL "))       handleGol(texto, chatId)
      else if (upper.startsWith("PARADAS "))   handleParadas(texto, chatId)
      else if (upper.startsWith("CONTEXTO "))  handleContexto(texto, chatId)
      else if (upper.startsWith("EXTRAS "))    handleExtras(texto, chatId)
      else if (upper.startsWith("1V1 "))       handle1v1(texto, chatId)
      else if (upper.startsWith("JUDO "))      handleJudo(texto, chatId)
      else if (upper.startsWith("CLUB "))      handleClub(texto, chatId)
      else if (upper.startsWith("ACADEMIA "))  handleAcademia(texto, chatId)
      else if (upper.startsWith("PESO "))      handlePeso(texto, chatId)
      else if (upper == "APP")                 handleApp(chatId)
      else if (upper == "SALTAR")              handleSaltar(chatId)
      else if (upper == "LISTO")               handleListo(chatId)
      else if (upper == "SI" || upper == "SÍ" || upper == "NO") handleSiNo(if (upper == "NO") "NO" else "SI", chatId)
      else if (upper == "NINGUNO")             handleNinguno(chatId)
      else if (upper == "ESTADO")              handleEstado(chatId)
      else if (upper == "AYUDA")               handleAyuda(chatId)
      else handleDesconocido(chatId)
    } catch { case e: Exception =>
      println(s"[Telegram bot] ERROR: ${e.getMessage.take(200)}")
      "⚠️ No he podido guardar ese dato. Revisa el formato (AYUDA) o regístralo en la app."
    }
  }

  private def tgArgs(texto: String): List[String] = texto.trim.split("\\s+").toList.drop(1)

  // ── SUEÑO ─────────────────────────────────────────────────────────────────
  private def handleSueno(texto: String, chatId: String): String = {
    val a = tgArgs(texto)
    val horas = a.headOption.flatMap(tgNum).filter(h => h > 0 && h <= 16)
    if (horas.isEmpty) return "Formato: SUEÑO [horas] [profundo min] [ligero min] [despierto min] [energía 1-5] [ánimo 1-5]\nEjemplo: SUEÑO 9 95 180 10 4 5\nO simplemente: SUEÑO 9"
    val profundo = a.lift(1).flatMap(tgInt).filter(_ >= 0)
    val ligero = a.lift(2).flatMap(tgInt).filter(_ >= 0)
    val despierto = a.lift(3).flatMap(tgInt).filter(_ >= 0)
    val energia = tgEnRango(a.lift(4).flatMap(tgInt), 1, 5)
    val animo = tgEnRango(a.lift(5).flatMap(tgInt), 1, 5)
    val conn = getConnection()
    try {
      // Upsert que no pisa lo ya registrado hoy desde la app (FC, dolor, notas...)
      val ps = conn.prepareStatement("""
        INSERT INTO wellness (fecha, horas_sueno, sueno_profundo_min, sueno_ligero_min, sueno_despierto_min, energia, animo)
        VALUES (CURRENT_DATE, ?, ?, ?, ?, ?, ?)
        ON CONFLICT (fecha) DO UPDATE SET horas_sueno = EXCLUDED.horas_sueno,
          sueno_profundo_min = COALESCE(EXCLUDED.sueno_profundo_min, wellness.sueno_profundo_min),
          sueno_ligero_min = COALESCE(EXCLUDED.sueno_ligero_min, wellness.sueno_ligero_min),
          sueno_despierto_min = COALESCE(EXCLUDED.sueno_despierto_min, wellness.sueno_despierto_min),
          energia = COALESCE(EXCLUDED.energia, wellness.energia),
          animo = COALESCE(EXCLUDED.animo, wellness.animo)""")
      ps.setDouble(1, horas.get)
      Seq(profundo, ligero, despierto, energia, animo).zipWithIndex.foreach { case (v, i) =>
        v match { case Some(x) => ps.setInt(i + 2, x); case None => ps.setNull(i + 2, java.sql.Types.INTEGER) }
      }
      ps.executeUpdate()
    } finally { conn.close() }
    val forma = calcularFormaHoy()("indiceForma").asInstanceOf[Double]
    val horasTxt = if (horas.get % 1 == 0) f"${horas.get}%.0fh" else f"${horas.get}%.1fh"
    s"✅ Sueño registrado: $horasTxt" + profundo.map(p => s" · ${p}min profundo").getOrElse("") +
      f" · Índice de Forma: $forma%.1f ${formaSemaforo(forma)}"
  }

  // ── FC ────────────────────────────────────────────────────────────────────
  private def handleFC(texto: String, chatId: String): String = {
    val bpm = tgEnRango(tgArgs(texto).headOption.flatMap(tgInt), 30, 200)
    if (bpm.isEmpty) return "Formato: FC [bpm]\nEjemplo: FC 58"
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement(
        "INSERT INTO wellness (fecha, fc_reposo) VALUES (CURRENT_DATE, ?) ON CONFLICT (fecha) DO UPDATE SET fc_reposo = EXCLUDED.fc_reposo")
      ps.setInt(1, bpm.get); ps.executeUpdate()
      val rs = conn.createStatement().executeQuery("SELECT AVG(fc_reposo) as m FROM wellness WHERE fc_reposo IS NOT NULL")
      val media = if (rs.next()) rs.getDouble("m") else 0.0
      f"✅ FC registrada: ${bpm.get} BPM · Media histórica: $media%.0f BPM" +
        avisosRPEconFCHoy().map("\n" + _).mkString
    } finally { conn.close() }
  }

  // ── PARTIDO (flujo por pasos) ─────────────────────────────────────────────
  private val tgPasosPartido = List("RUBRICA", "GOL", "PARADAS", "CONTEXTO", "EXTRAS", "1V1", "FACTOR")

  private def tgPrompt(paso: String, s: TgSesion): String = paso match {
    case "RUBRICA" => "Rúbrica (posición/decisiones/pies/comunicación/actitud, 1-5 cada una):\nRUBRICA 4 3 3 4 5\n(SALTAR para dejarlo para la app)"
    case "GOL" =>
      val n = s.golesRegistrados + 1
      (if (n == 1) s"Encajaste ${s.golesPendientes} ${if (s.golesPendientes == 1) "gol" else "goles"}. GOL 1:\n" else s"GOL $n de ${s.golesPendientes}:\n") +
        "GOL [zona] [situación] [posición] [cuarto]\n" +
        "Zonas: ALTO_DER/ALTO_IZQ/ALTO_CEN/BAJO_DER/BAJO_IZQ/BAJO_CEN\n" +
        "Situaciones: REMATE/CABEZA/FALLO/1V1/PENALTI/FALTA\n" +
        "Posición: BIEN_PLANTADO/PASO_NEGATIVO/DESPLAZAMIENTO_TARDIO/IMPARABLE\n" +
        "Cuartos: Q1/Q2/Q3/Q4\nEjemplo: GOL BAJO_DER 1V1 PASO_NEGATIVO Q2"
    case "PARADAS" => "Paradas y cantera:\nPARADAS [total] [1v1] [aéreas] [con el pie] [scanning efectivo] [córners dominados] [córners cedidos]\nEjemplo: PARADAS 4 2 1 3 2 3 1"
    case "CONTEXTO" => "Contexto:\nCONTEXTO [CASA/FUERA] [min calentamiento] [NAT/ART/TIERRA/INTERIOR] [INM/NORM/LENTO] [economía 1-5] [calidad decisión 0-100]\nEjemplo: CONTEXTO FUERA 12 ART NORM 4 75"
    case "EXTRAS" => "Extras:\nEXTRAS [autopercepción Héctor 1-5] [conducta padre 1-5] [rutina: SI/NO] [regulación: HABLA_SOLO/RESPIRA/ENFADO/NEUTRAL/REORGANIZA/DECAIDO]\nEjemplo: EXTRAS 4 4 SI RESPIRA"
    case "1V1" => "Ángulo en 1v1 (SALTAR si no lo recuerdas):\n1V1 [central paradas] [central goles] [izq paradas] [izq goles] [der paradas] [der goles]\nEjemplo: 1V1 2 0 0 1 0 0"
    case "FACTOR" => "¿Algún factor externo relevante hoy? (texto libre o NINGUNO)"
    case _ => ""
  }

  /** Pasa al siguiente paso del flujo de partido (saltando GOL si no quedan goles) o lo cierra si no hay mas. */
  private def tgSiguientePaso(chatId: String, s: TgSesion, desde: String, prefijo: String): String = {
    val restantes = tgPasosPartido.dropWhile(_ != desde).drop(1)
    restantes.find(p => p != "GOL" || s.golesRegistrados < s.golesPendientes) match {
      case Some(p) =>
        val nueva = s.copy(paso = Some(p))
        tgGuardarSesion(chatId, nueva)
        prefijo + tgPrompt(p, nueva)
      case None => tgFinalizarPartido(chatId, s)
    }
  }

  private def tgPartidoEnCurso(s: TgSesion): Option[Int] = if (s.flujo.contains("PARTIDO")) s.matchId else None
  private val tgSinPartido = "No hay ningún partido en curso. Empieza con:\nPARTIDO [rival] [GF]-[GC] [nota]\nEjemplo: PARTIDO Rivas 2-1 7.5"

  private def handlePartidoStep1(texto: String, chatId: String): String = {
    val patron = """(?i)^PARTIDO\s+(.+?)\s+(\d{1,2})\s*-\s*(\d{1,2})\s+(\d{1,2}(?:[.,]\d+)?)\s*$""".r
    texto.trim match {
      case patron(rivalRaw, gfS, gcS, notaS) =>
        val nota = tgNum(notaS).filter(n => n >= 0 && n <= 10)
        if (nota.isEmpty) return "La nota debe estar entre 0 y 10.\nEjemplo: PARTIDO Rivas 2-1 7.5"
        val rival = fixEncoding(rivalRaw.trim)
        val (gf, gc) = (gfS.toInt, gcS.toInt)
        val matchId = quickSaveMatch(rival, gf, gc, nota.get)
        if (matchId <= 0) return "⚠️ No se pudo crear el partido. ¿Hay alguna temporada creada?"
        val s = TgSesion(flujo = Some("PARTIDO"), paso = Some("RUBRICA"), matchId = Some(matchId), golesPendientes = gc)
        tgGuardarSesion(chatId, s)
        f"✅ $rival $gf-$gc · Nota ${nota.get}%.1f\n" + tgPrompt("RUBRICA", s)
      case _ => "Formato: PARTIDO [rival] [GF]-[GC] [nota]\nEjemplo: PARTIDO Rivas 2-1 7.5"
    }
  }

  private def handleRubrica(texto: String, chatId: String): String = {
    val s = tgSesion(chatId)
    val matchId = tgPartidoEnCurso(s).getOrElse(return tgSinPartido)
    val v = tgArgs(texto).map(x => tgEnRango(tgInt(x), 1, 5))
    if (v.size != 5 || v.exists(_.isEmpty)) return "Necesito 5 valores del 1 al 5 (posición, decisiones, pies, comunicación, actitud).\nEjemplo: RUBRICA 4 3 3 4 5"
    val r = v.flatten
    updateRubricaMatch(matchId, r(0), r(1), r(2), r(3), r(4))
    tgSiguientePaso(chatId, s, "RUBRICA", "✅ Rúbrica guardada\n")
  }

  private def handleGol(texto: String, chatId: String): String = {
    val s = tgSesion(chatId)
    val matchId = tgPartidoEnCurso(s).getOrElse(return tgSinPartido)
    if (s.golesRegistrados >= s.golesPendientes) return "Ya están registrados todos los goles de este partido."
    val a = tgArgs(texto).map(_.toUpperCase)
    val zona = a.headOption.flatMap(tgZonas.get)
    if (zona.isEmpty) return tgPrompt("GOL", s)
    val situacion = a.lift(1).flatMap(tgSituaciones.get)
    val posicion = a.lift(2).filter(tgPosiciones.contains)
    val minuto = a.lift(3).flatMap(tgCuartos.get).getOrElse(0)
    val (origen, sit) = situacion.getOrElse(("Otro", "Otro"))
    val responsabilidad = posicion match { case Some("IMPARABLE") | Some("BIEN_PLANTADO") => "Ninguna"; case Some(_) => "Media"; case None => "Media" }
    val parable = if (posicion.contains("IMPARABLE")) "No" else "Dudoso"
    saveMatchGoal(matchId, minuto, origen, sit, responsabilidad, parable, zona.get, posicion.map(p => s"POS:$p").getOrElse("Telegram"))
    // Los mapas de goles existentes (getGoalHeatmap, biomecanica) leen matches.zona_goles, no match_goals
    val connZ = getConnection()
    try {
      val psZ = connZ.prepareStatement(
        "UPDATE matches SET zona_goles = CASE WHEN COALESCE(zona_goles, '') = '' THEN ? ELSE zona_goles || ',' || ? END WHERE id = ?")
      psZ.setString(1, zona.get); psZ.setString(2, zona.get); psZ.setInt(3, matchId); psZ.executeUpdate()
    } finally { connZ.close() }

    val conn = getConnection()
    try {
      // Cuartos (solo goles con minuto) y posicion mas repetida en los goles del partido
      val psMin = conn.prepareStatement("SELECT minuto FROM match_goals WHERE match_id = ? AND minuto > 0")
      psMin.setInt(1, matchId)
      val rsMin = psMin.executeQuery()
      val minutos = Iterator.continually(rsMin).takeWhile(_.next()).map(_.getInt("minuto")).toList
      if (minutos.nonEmpty) saveMinutoGoles(matchId, minutos)
      val psPos = conn.prepareStatement("""
        SELECT SUBSTRING(notas FROM 'POS:([A-Z_]+)') as p, COUNT(*) as n FROM match_goals
        WHERE match_id = ? AND notas LIKE 'POS:%' GROUP BY 1 ORDER BY n DESC LIMIT 1""")
      psPos.setInt(1, matchId)
      val rsPos = psPos.executeQuery()
      if (rsPos.next()) tgActualizarPartido(matchId, Seq("posicion_set" -> rsPos.getString("p")))
    } finally { conn.close() }

    val nueva = s.copy(golesRegistrados = s.golesRegistrados + 1)
    tgGuardarSesion(chatId, nueva)
    if (nueva.golesRegistrados < nueva.golesPendientes) s"✅ Gol ${nueva.golesRegistrados} guardado\n" + tgPrompt("GOL", nueva)
    else tgSiguientePaso(chatId, nueva, "GOL", "✅ Goles registrados\n")
  }

  private def handleParadas(texto: String, chatId: String): String = {
    val s = tgSesion(chatId)
    val matchId = tgPartidoEnCurso(s).getOrElse(return tgSinPartido)
    val v = tgArgs(texto).map(x => tgInt(x).filter(_ >= 0))
    if (v.isEmpty || v.exists(_.isEmpty) || v.size > 7) return tgPrompt("PARADAS", s)
    val columnas = List("paradas", "paradas_1v1", "paradas_aereas", "acciones_pie", "scanning_efectivo", "corners_dominados", "corners_cedidos")
    tgActualizarPartido(matchId, columnas.zip(v.flatten))
    tgSiguientePaso(chatId, s, "PARADAS", "✅ Paradas guardadas\n")
  }

  private def handleContexto(texto: String, chatId: String): String = {
    val s = tgSesion(chatId)
    val matchId = tgPartidoEnCurso(s).getOrElse(return tgSinPartido)
    val a = tgArgs(texto).map(_.toUpperCase)
    val local = a.headOption.collect { case "CASA" => true; case "FUERA" => false }
    val calentamiento = a.lift(1).flatMap(tgInt).filter(m => m >= 0 && m <= 120)
    val superficie = a.lift(2).collect { case "NAT" | "NATURAL" => "NATURAL"; case "ART" | "ARTIFICIAL" => "ARTIFICIAL"; case "TIERRA" => "TIERRA"; case "INTERIOR" => "INTERIOR" }
    val velocidad = a.lift(3).collect { case "INM" | "INMEDIATO" => "INMEDIATO"; case "NORM" | "NORMAL" => "NORMAL"; case "LENTO" => "LENTO" }
    val economia = tgEnRango(a.lift(4).flatMap(tgInt), 1, 5)
    val calidad = tgEnRango(a.lift(5).flatMap(tgInt), 0, 100)
    val esperados = Seq(local, calentamiento, superficie, velocidad, economia, calidad).take(a.size)
    if (a.isEmpty || esperados.exists(_.isEmpty)) return "No entendí algún valor.\n" + tgPrompt("CONTEXTO", s)
    tgActualizarPartido(matchId, Seq(
      local.map("es_local" -> _), calentamiento.map("calentamiento_min" -> _), superficie.map("superficie" -> _),
      velocidad.map("velocidad_distribucion" -> _), economia.map("economia_movimiento" -> _), calidad.map("calidad_decision_pct" -> _)).flatten)
    tgSiguientePaso(chatId, s, "CONTEXTO", "✅ Contexto guardado\n")
  }

  private def handleExtras(texto: String, chatId: String): String = {
    val s = tgSesion(chatId)
    val matchId = tgPartidoEnCurso(s).getOrElse(return tgSinPartido)
    val a = tgArgs(texto).map(_.toUpperCase)
    val autop = tgEnRango(a.headOption.flatMap(tgInt), 1, 5)
    val padre = tgEnRango(a.lift(1).flatMap(tgInt), 1, 5)
    val rutina = a.lift(2).collect { case "SI" | "SÍ" => "SI"; case "NO" => "NO"; case "SIN_RUTINA" => "SIN_RUTINA" }
    val regulacion = a.lift(3).filter(tgRegulaciones.contains)
    val esperados = Seq(autop, padre, rutina, regulacion).take(a.size)
    if (a.isEmpty || esperados.exists(_.isEmpty)) return "No entendí algún valor.\n" + tgPrompt("EXTRAS", s)
    tgActualizarPartido(matchId, Seq(
      autop.map("autopercepcion_prepartido" -> _), padre.map("conducta_padre" -> _),
      rutina.map("rutina_prepartido" -> _), regulacion.map("regulacion_emocional" -> _)).flatten)
    tgSiguientePaso(chatId, s, "EXTRAS", "✅ Extras guardados\n")
  }

  private def handle1v1(texto: String, chatId: String): String = {
    val s = tgSesion(chatId)
    val matchId = tgPartidoEnCurso(s).getOrElse(return tgSinPartido)
    val v = tgArgs(texto).map(x => tgInt(x).filter(_ >= 0))
    if (v.size != 6 || v.exists(_.isEmpty)) return tgPrompt("1V1", s)
    // Mismo JSON que genera el formulario web (adjustAngulo1v1)
    val claves = List("central_ok", "central_gc", "izq_ok", "izq_gc", "der_ok", "der_gc")
    val json = ujson.write(ujson.Obj.from(claves.zip(v.flatten).map { case (k, n) => k -> ujson.Num(n) }))
    tgActualizarPartido(matchId, Seq("angulo_1v1_data" -> json))
    tgSiguientePaso(chatId, s, "1V1", "✅ 1v1 guardado\n")
  }

  private def handleFactor(texto: String, chatId: String, s: TgSesion): String = {
    val matchId = tgPartidoEnCurso(s).getOrElse(return tgSinPartido)
    tgActualizarPartido(matchId, Seq("factores_externos" -> fixEncoding(texto.take(500))))
    tgFinalizarPartido(chatId, s)
  }

  private def tgFinalizarPartido(chatId: String, s: TgSesion): String = {
    val matchId = tgPartidoEnCurso(s).getOrElse(return tgSinPartido)
    tgLimpiarSesion(chatId)
    val cpi = try { val c = calcularCPI(matchId); tgActualizarPartido(matchId, Seq("cpi" -> c)); Some(c) } catch { case _: Exception => None }
    new Thread(() => detectarHitos()).start()
    generarGuiaConversacion(matchId)

    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("SELECT * FROM matches WHERE id = ?")
      ps.setInt(1, matchId)
      val r = ps.executeQuery()
      if (!r.next()) return "✅ Partido guardado."
      def optInt(c: String) = Option(r.getObject(c)).map(_ => r.getInt(c))
      def txt(c: String) = Option(r.getString(c)).filter(_.nonEmpty)
      val lugar = Option(r.getObject("es_local")).map(_ => if (r.getBoolean("es_local")) "CASA" else "FUERA").getOrElse("—")
      val rubrica = Seq("Pos" -> "rubrica_posicion", "Dec" -> "rubrica_decisiones", "Pie" -> "rubrica_pies",
        "Com" -> "rubrica_comunicacion", "Act" -> "rubrica_actitud").map { case (e, c) => e + optInt(c).map(_.toString).getOrElse("—") }.mkString(" ")
      val resumen = List(
        "✅ PARTIDO COMPLETO GUARDADO 🏆", "",
        s"⚽ ${fixEncoding(r.getString("rival"))} | ${r.getInt("goles_favor")}-${r.getInt("goles_contra")} | $lugar",
        f"⭐ Nota: ${r.getDouble("nota")}%.1f | CPI: ${cpi.map(c => f"$c%.1f").getOrElse("—")}",
        s"🧤 Paradas: ${r.getInt("paradas")} (${r.getInt("paradas_1v1")} en 1v1, ${r.getInt("paradas_aereas")} aéreas)")
      val tail = List(
        s"📊 $rubrica",
        s"🔄 Rutina: ${txt("rutina_prepartido").getOrElse("—")} · Regulación: ${txt("regulacion_emocional").getOrElse("—")}",
        s"${TelegramService.baseUrl.replaceFirst("^https?://", "")}/history")

      val psG = conn.prepareStatement("SELECT zona_gol, minuto FROM match_goals WHERE match_id = ? ORDER BY id")
      psG.setInt(1, matchId)
      val rg = psG.executeQuery()
      val zonaNombre = tgZonas.map(_.swap)
      val goles = Iterator.continually(rg).takeWhile(_.next()).map { g =>
        val z = Option(g.getString("zona_gol")).flatMap(zonaNombre.get).getOrElse("?")
        val m = g.getInt("minuto")
        val q = if (m <= 0) "" else if (m <= 12) " Q1" else if (m <= 25) " Q2" else if (m <= 37) " Q3" else " Q4"
        z + q
      }.toList
      val golesLinea = if (r.getInt("goles_contra") == 0) List("⬛ Goles: ninguno 🧤") else List(s"⬛ Goles: ${if (goles.isEmpty) "sin detalle" else goles.mkString(", ")}")
      (resumen ++ golesLinea ++ tail).mkString("\n")
    } finally { conn.close() }
  }

  // ── JUDO / CLUB / ACADEMIA ────────────────────────────────────────────────
  private def tgAcwrTexto(): String = {
    val e = calcularACWRConEstado()
    if (e("status").asInstanceOf[String] == "INSUFICIENTE") "acumulando datos" else f"${e("acwr").asInstanceOf[Double]}%.2f"
  }

  private def tgAusencia(tipo: String, motivoRaw: Option[String]): String = {
    val motivo = motivoRaw.map(_.toUpperCase).filter(m => Set("ENFERMEDAD", "FAMILIAR", "DESCANSO", "LESION", "OTRO").contains(m)).getOrElse("OTRO")
    logTraining(tipo, "", 0, 0, 0, "", tipoAusencia = Some(motivo))
    s"✅ Ausencia de $tipo registrada ($motivo)"
  }

  private def handleJudo(texto: String, chatId: String): String = {
    val a = tgArgs(texto)
    if (a.headOption.exists(_.equalsIgnoreCase("NO"))) return tgAusencia("Judo", a.lift(1))
    val minutos = a.headOption.flatMap(tgInt).filter(m => m > 0 && m <= 300)
    val rpe = tgEnRango(a.lift(1).flatMap(tgInt), 1, 10)
    if (minutos.isEmpty || rpe.isEmpty) return "Formato: JUDO [duración min] [RPE 1-10]\nEjemplo: JUDO 60 6\nO si no fue: JUDO NO [ENFERMEDAD/FAMILIAR/DESCANSO/OTRO]"
    // calidad/atencion fijas como en el formulario web: el padre no las observa en judo
    val idJudo = logTraining("Judo", "", rpe.get, 3, 3, "", duracionMin = minutos)
    tgProgramarPreguntaRpe(idJudo)
    new Thread(() => detectarHitos()).start()
    s"✅ Judo registrado: ${minutos.get}min · RPE ${rpe.get} · ACWR: ${tgAcwrTexto()}"
  }

  private def handleClub(texto: String, chatId: String): String = tgEntrenoConFeedback("Club", "CLUB", texto, chatId)
  private def handleAcademia(texto: String, chatId: String): String = tgEntrenoConFeedback("Academia", "ACADEMIA", texto, chatId)

  private def tgEntrenoConFeedback(tipo: String, flujo: String, texto: String, chatId: String): String = {
    val a = tgArgs(texto)
    if (a.headOption.exists(_.equalsIgnoreCase("NO"))) { tgLimpiarSesion(chatId); return tgAusencia(tipo, a.lift(1)) }
    val minutos = a.headOption.flatMap(tgInt).filter(m => m > 0 && m <= 300)
    val rpe = tgEnRango(a.lift(1).flatMap(tgInt), 1, 10)
    val atencion = tgEnRango(a.lift(2).flatMap(tgInt), 1, 5)
    val calidad = tgEnRango(a.lift(3).flatMap(tgInt), 1, 5)
    if (minutos.isEmpty || rpe.isEmpty || (a.size > 2 && atencion.isEmpty) || (a.size > 3 && calidad.isEmpty))
      return s"Formato: $flujo [duración min] [RPE 1-10] [atención 1-5] [calidad 1-5]\nEjemplo: $flujo ${if (flujo == "CLUB") "75 7 4 4" else "60 6 5 4"}\nO si no fue: $flujo NO [motivo]"
    // La app guarda calidad/atencion en escala 1-10: el 1-5 del bot se duplica
    val id = logTraining(tipo, "", rpe.get, calidad.getOrElse(3) * 2, atencion.getOrElse(3) * 2, "", duracionMin = minutos)
    tgProgramarPreguntaRpe(id)
    new Thread(() => detectarHitos()).start()
    tgGuardarSesion(chatId, TgSesion(flujo = Some(flujo), paso = Some("FEEDBACK"), trainingId = Some(id)))
    s"✅ $tipo guardado. ¿Feedback del entrenador? (texto libre o NINGUNO)"
  }

  private def handleFeedback(texto: String, chatId: String, s: TgSesion): String = {
    val trainingId = s.trainingId.getOrElse { tgLimpiarSesion(chatId); return "No hay ningún entreno en curso." }
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("UPDATE trainings SET feedback_entrenador = ? WHERE id = ?")
      ps.setString(1, fixEncoding(texto.take(2000))); ps.setInt(2, trainingId); ps.executeUpdate()
    } finally { conn.close() }

    val detectadas = detectarSkillsEnFeedback(texto)
    val antes = getSugerenciasSkillPendientes().map(_("skillId").asInstanceOf[Int]).toSet
    guardarSugerenciasSkillDesdeFeedback(texto)
    val nuevas = getSugerenciasSkillPendientes().filterNot(x => antes.contains(x("skillId").asInstanceOf[Int]))
    val idp = if (s.flujo.contains("ACADEMIA")) tgRelacionIdp(detectadas) else None

    if (nuevas.nonEmpty) {
      tgGuardarSesion(chatId, s.copy(paso = Some("SKILLS:" + nuevas.map(_("skillId")).mkString(","))))
      s"Guardian detectó: ${nuevas.map(_("habilidad")).mkString(", ")}\n¿Actualizar checklist? SI/NO" + idp.map(i => s"\n💡 Relacionado con IDP: $i").getOrElse("")
    } else {
      tgLimpiarSesion(chatId)
      tgResumenEntreno(trainingId) + idp.map(i => s"\n💡 Relacionado con IDP: $i").getOrElse("")
    }
  }

  /** Objetivo del IDP activo que menciona alguna de las skills detectadas en el feedback. */
  private def tgRelacionIdp(detectadas: List[String]): Option[String] = {
    if (detectadas.isEmpty) return None
    val palabras = detectadas.flatMap(d => skillFeedbackKeywords.getOrElse(d, Nil)).map(_.toLowerCase)
    getActiveIdpTemporada().flatMap { t =>
      getIdpObjetivos(t("id").asInstanceOf[Int]).find { o =>
        val txt = (o("objetivo").toString + " " + o("dimension").toString + " " + o("metrica").toString).toLowerCase
        palabras.exists(txt.contains)
      }.map(_("objetivo").toString)
    }
  }

  private def tgResumenEntreno(trainingId: Int): String = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("SELECT tipo, rpe, calidad, atencion, duracion_min, feedback_entrenador FROM trainings WHERE id = ?")
      ps.setInt(1, trainingId)
      val r = ps.executeQuery()
      if (!r.next()) return "✅ Entreno guardado."
      val dur = Option(r.getObject("duracion_min")).map(_ => s"${r.getInt("duracion_min")}min · ").getOrElse("")
      val fb = Option(r.getString("feedback_entrenador")).filter(_.nonEmpty).map(f => s"\n📝 Feedback: ${f.take(300)}").getOrElse("")
      s"✅ ${r.getString("tipo")} registrado: ${dur}RPE ${r.getInt("rpe")} · Atención ${r.getInt("atencion") / 2}/5 · Calidad ${r.getInt("calidad") / 2}/5$fb\n📈 ACWR: ${tgAcwrTexto()}"
    } finally { conn.close() }
  }

  // ── PESO ──────────────────────────────────────────────────────────────────
  private def handlePeso(texto: String, chatId: String): String = {
    val a = tgArgs(texto)
    val kg = a.headOption.flatMap(tgNum).filter(k => k >= 10 && k <= 150)
    if (kg.isEmpty) return "Formato: PESO [kg] o con báscula: PESO [kg] [músculo kg] [masa ósea kg]\nEjemplo: PESO 27.3\nCon báscula: PESO 27.3 19.2 1.1"
    val musculo = a.lift(1).flatMap(tgNum).filter(_ > 0)
    val osea = a.lift(2).flatMap(tgNum).filter(_ > 0)
    val conn = getConnection()
    try {
      val rsAnt = conn.createStatement().executeQuery(
        "SELECT peso, fecha FROM physical_growth WHERE peso > 0 ORDER BY fecha DESC, id DESC LIMIT 1")
      val anterior = if (rsAnt.next()) Some((rsAnt.getDouble("peso"), rsAnt.getDate("fecha").toLocalDate)) else None
      // La fila de crecimiento necesita altura: se arrastra la ultima medida (sin velocidad de crecimiento nueva)
      val rsAlt = conn.createStatement().executeQuery(
        "SELECT altura FROM physical_growth WHERE altura > 0 ORDER BY fecha DESC, id DESC LIMIT 1")
      val altura = if (rsAlt.next()) Some(rsAlt.getDouble("altura")) else None
      val ps = conn.prepareStatement(
        "INSERT INTO physical_growth (fecha, altura, peso, velocidad_crecimiento, kg_musculo, kg_masa_osea) VALUES (CURRENT_DATE, ?, ?, 0, ?, ?)")
      altura match { case Some(h) => ps.setDouble(1, h); case None => ps.setNull(1, java.sql.Types.DOUBLE) }
      ps.setDouble(2, kg.get)
      musculo match { case Some(v) => ps.setDouble(3, v); case None => ps.setNull(3, java.sql.Types.DOUBLE) }
      osea match { case Some(v) => ps.setDouble(4, v); case None => ps.setNull(4, java.sql.Types.DOUBLE) }
      ps.executeUpdate()
      anterior match {
        case Some((p, f)) =>
          val dias = java.time.temporal.ChronoUnit.DAYS.between(f, LocalDate.now())
          val diff = kg.get - p
          f"✅ Peso: ${kg.get}%.1fkg · Anterior: $p%.1fkg (${if (diff >= 0) "+" else ""}$diff%.1fkg en $dias días)"
        case None => f"✅ Peso: ${kg.get}%.1fkg (primer registro)"
      }
    } finally { conn.close() }
  }

  // ── COMANDOS GLOBALES ─────────────────────────────────────────────────────
  private def handleApp(chatId: String): String = {
    tgLimpiarSesion(chatId)
    s"📱 ${TelegramService.baseUrl}"
  }

  private def handleSaltar(chatId: String): String = {
    val s = tgSesion(chatId)
    s.paso match {
      case Some("RPE_HECTOR") => tgLimpiarSesion(chatId); "⏭️ Sin RPE de Héctor para este entreno."
      case Some(p) if s.flujo.contains("PARTIDO") => tgSiguientePaso(chatId, s, p, "⏭️ Saltado\n")
      case Some(p) if p == "FEEDBACK" || p.startsWith("SKILLS:") => handleNinguno(chatId)
      case _ => "No hay ningún registro en curso."
    }
  }

  private def handleListo(chatId: String): String = {
    val s = tgSesion(chatId)
    s.flujo match {
      case Some("PARTIDO") => tgFinalizarPartido(chatId, s)
      case Some(_) => s.trainingId.map { id => tgLimpiarSesion(chatId); tgResumenEntreno(id) }.getOrElse { tgLimpiarSesion(chatId); "✅ Listo." }
      case None => "No hay ningún registro en curso."
    }
  }

  private def handleSiNo(respuesta: String, chatId: String): String = {
    val s = tgSesion(chatId)
    s.paso.filter(_.startsWith("SKILLS:")) match {
      case Some(p) =>
        val ids = p.stripPrefix("SKILLS:").split(",").flatMap(_.toIntOption)
        if (respuesta == "SI") ids.foreach(confirmarSkillDesdeSugerencia) else ids.foreach(descartarSugerenciaSkill)
        tgLimpiarSesion(chatId)
        (if (respuesta == "SI") "✅ Checklist actualizado\n" else "👌 Checklist sin cambios\n") + s.trainingId.map(tgResumenEntreno).getOrElse("")
      case None => "No hay ninguna pregunta pendiente."
    }
  }

  private def handleNinguno(chatId: String): String = {
    val s = tgSesion(chatId)
    s.paso match {
      case Some("FACTOR") => tgFinalizarPartido(chatId, s)
      case Some(p) if p == "FEEDBACK" || p.startsWith("SKILLS:") =>
        tgLimpiarSesion(chatId)
        s.trainingId.map(tgResumenEntreno).getOrElse("✅ Listo.")
      case _ => "No hay ningún campo opcional pendiente."
    }
  }

  private def handleEstado(chatId: String): String = {
    val conn = getConnection()
    try {
      val w = conn.createStatement().executeQuery(
        "SELECT horas_sueno, sueno_profundo_min, fc_reposo FROM wellness WHERE fecha = CURRENT_DATE")
      val (sueno, fc) = if (w.next()) {
        val h = w.getDouble("horas_sueno")
        val prof = Option(w.getObject("sueno_profundo_min")).map(_ => s" (${w.getInt("sueno_profundo_min")} min profundo)").getOrElse("")
        (if (h > 0) f"$h%.1fh$prof" else "—", Option(w.getObject("fc_reposo")).map(_ => s"${w.getInt("fc_reposo")} BPM").getOrElse("—"))
      } else ("—", "—")
      val t = conn.createStatement().executeQuery(
        "SELECT tipo, rpe, duracion_min, tipo_ausencia FROM trainings WHERE fecha = CURRENT_DATE ORDER BY id")
      val entrenos = Iterator.continually(t).takeWhile(_.next()).map { r =>
        Option(r.getString("tipo_ausencia")).map(m => s"${r.getString("tipo")} (no fue: $m)")
          .getOrElse(s"${r.getString("tipo")}${Option(r.getObject("duracion_min")).map(_ => s" ${r.getInt("duracion_min")}min").getOrElse("")} RPE ${r.getInt("rpe")}")
      }.toList
      val m = conn.createStatement().executeQuery(
        "SELECT rival, goles_favor, goles_contra, nota FROM matches WHERE status = 'PLAYED' AND fecha = CURRENT_DATE ORDER BY id DESC LIMIT 1")
      val partido = if (m.next()) f"${fixEncoding(m.getString("rival"))} ${m.getInt("goles_favor")}-${m.getInt("goles_contra")} (${m.getDouble("nota")}%.1f)" else "—"
      val p = conn.createStatement().executeQuery("SELECT peso FROM physical_growth WHERE fecha = CURRENT_DATE AND peso > 0 ORDER BY id DESC LIMIT 1")
      val peso = if (p.next()) f"${p.getDouble("peso")}%.1fkg" else "—"
      val forma = calcularFormaHoy()("indiceForma").asInstanceOf[Double]
      val enCurso = tgSesion(chatId).flujo.map(f => s"\n⏳ Registro en curso: $f (LISTO para cerrarlo)").getOrElse("")
      s"📋 Registros de hoy (${LocalDate.now()}):\n💤 Sueño: $sueno\n❤️ FC: $fc\n🏃 Entrenos: ${if (entrenos.isEmpty) "—" else entrenos.mkString(", ")}\n🏟️ Partido: $partido\n⚖️ Peso: $peso\n" +
        f"📊 Índice de Forma: $forma%.1f ${formaSemaforo(forma)}" + enCurso
    } finally { conn.close() }
  }

  private def handleAyuda(chatId: String): String =
    """Comandos disponibles:
      |
      |💤 SUEÑO 9 95 180 10 4 5
      |❤️ FC 58
      |🏟️ PARTIDO Rivas 2-1 7.5
      |🥋 JUDO 60 6
      |⚽ CLUB 75 7 4 4
      |🎓 ACADEMIA 60 6 5 4
      |⚖️ PESO 27.3
      |
      |APP → ir a la app web
      |ESTADO → registros de hoy
      |AYUDA → este mensaje""".stripMargin

  private def handleDesconocido(chatId: String): String = "🤔 No he entendido el mensaje. Escribe AYUDA para ver los comandos."

  // ── BLOQUE E2: RPE DE HECTOR 2 HORAS DESPUES DEL ENTRENO ─────────────────
  private def tgProgramarPreguntaRpe(trainingId: Int): Unit = {
    if (trainingId <= 0) return
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement(
        "INSERT INTO feature_cache (cache_key, payload, updated_at) VALUES (?, ?, NOW()) ON CONFLICT (cache_key) DO NOTHING")
      ps.setString(1, s"tg_rpe_pend_$trainingId"); ps.setString(2, trainingId.toString); ps.executeUpdate()
    } finally { conn.close() }
  }

  /** Pregunta pendiente cuyo entreno se registro hace >= 2h. Solo si el chat no esta en mitad de otro flujo. */
  private def tgPreguntaRpePendiente(chatId: String): Option[String] = {
    if (tgSesion(chatId).flujo.isDefined) return None
    val conn = getConnection()
    try {
      val rs = conn.createStatement().executeQuery("""
        SELECT f.cache_key, t.id, t.tipo FROM feature_cache f
        JOIN trainings t ON t.id = CAST(f.payload AS INT)
        WHERE f.cache_key LIKE 'tg_rpe_pend_%' AND f.updated_at <= NOW() - INTERVAL '2 hours'
        ORDER BY f.updated_at ASC LIMIT 1""")
      if (!rs.next()) return None
      val (clave, id, tipo) = (rs.getString("cache_key"), rs.getInt("id"), rs.getString("tipo"))
      val del = conn.prepareStatement("DELETE FROM feature_cache WHERE cache_key = ?")
      del.setString(1, clave); del.executeUpdate()
      // entrenos de hace mas de un dia: la pregunta ya no tiene sentido
      val psF = conn.prepareStatement("SELECT fecha >= CURRENT_DATE - 1 as reciente FROM trainings WHERE id = ?")
      psF.setInt(1, id)
      val rf = psF.executeQuery()
      if (!rf.next() || !rf.getBoolean("reciente")) return None
      tgGuardarSesion(chatId, TgSesion(flujo = Some("RPE"), paso = Some("RPE_HECTOR"), trainingId = Some(id)))
      Some(s"😴 ¿Cómo llegó Héctor a casa del $tipo?\n1=Fresco · 2=Normal · 3=Algo cansado · 4=Muy cansado · 5=Agotado\nResponde solo el número o SALTAR")
    } finally { conn.close() }
  }

  private def handleRpeHector(valor: Int, chatId: String, s: TgSesion): String = {
    val id = s.trainingId.getOrElse { tgLimpiarSesion(chatId); return "No hay ningún entreno pendiente." }
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("UPDATE trainings SET rpe_hector = ? WHERE id = ?")
      ps.setInt(1, valor); ps.setInt(2, id); ps.executeUpdate()
    } finally { conn.close() }
    tgLimpiarSesion(chatId)
    s"✅ Registrado: Héctor llegó ${etiquetasRpeHector(valor - 1).toLowerCase} ($valor/5)"
  }

  // ── RECORDATORIOS PROGRAMADOS ─────────────────────────────────────────────
  /** true (y lo marca) si el recordatorio `clave` no se ha enviado en los ultimos `dias` dias (1 = hoy). */
  private def tgMarcarRecordatorio(clave: String, dias: Int = 1): Boolean = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement(
        "SELECT COUNT(*) as c FROM feature_cache WHERE cache_key = ? AND updated_at::date > CURRENT_DATE - (?::int)")
      ps.setString(1, s"tg_rec_$clave"); ps.setInt(2, dias)
      val rs = ps.executeQuery()
      if (rs.next() && rs.getInt("c") > 0) false
      else {
        val up = conn.prepareStatement(
          "INSERT INTO feature_cache (cache_key, payload, updated_at) VALUES (?, '1', NOW()) ON CONFLICT (cache_key) DO UPDATE SET payload = '1', updated_at = NOW()")
        up.setString(1, s"tg_rec_$clave"); up.executeUpdate()
        true
      }
    } finally { conn.close() }
  }

  /**
   * Mensajes que tocan ahora (hora de Madrid). Se llama cada pocos minutos desde GuardianServer;
   * cada recordatorio se envia como mucho una vez por dia (FC: cada 3 dias).
   * Las sesiones esperadas salen de weekly_structure, nunca de dias fijos.
   */
  def tgRecordatoriosPendientes(): List[String] = {
    if (!TelegramService.configurado) return Nil
    val ahora = java.time.ZonedDateTime.now(java.time.ZoneId.of(sys.env.getOrElse("GUARDIAN_TZ", "Europe/Madrid")))
    val hora = ahora.getHour
    val hoy = ahora.toLocalDate
    val esLunes = hoy.getDayOfWeek == java.time.DayOfWeek.MONDAY
    val msgs = scala.collection.mutable.ListBuffer[String]()
    val conn = getConnection()
    try {
      def cuenta(sql: String): Int = { val rs = conn.createStatement().executeQuery(sql); if (rs.next()) rs.getInt(1) else 0 }
      val psE = conn.prepareStatement("SELECT tipo_sesion FROM weekly_structure WHERE activo = TRUE AND dia_semana = ?")
      psE.setInt(1, hoy.getDayOfWeek.getValue)
      val rsE = psE.executeQuery()
      val sesionesHoy = Iterator.continually(rsE).takeWhile(_.next()).map(_.getString("tipo_sesion")).toSet
      def pendiente(tipo: String) = sesionesHoy.contains(tipo) && !sesionRegistrada(conn, hoy, tipo)

      if (hora == 8) {
        if (cuenta("SELECT COUNT(*) FROM wellness WHERE fecha = CURRENT_DATE AND horas_sueno > 0") == 0 && tgMarcarRecordatorio("SUENO"))
          msgs += "Buenos días ☀️ ¿Cómo durmió Héctor anoche?\nSUEÑO [horas] [profundo min] [ligero min] [despierto min] [energía 1-5] [ánimo 1-5]\nEjemplo: SUEÑO 9 95 180 10 4 5\nO simplemente: SUEÑO 9"
        if (cuenta("SELECT COUNT(*) FROM wellness WHERE fc_reposo IS NOT NULL AND fecha > CURRENT_DATE - 3") == 0 && tgMarcarRecordatorio("FC", 3))
          msgs += "❤️ Sin datos de FC esta semana.\nFC [bpm]\nEjemplo: FC 58"
        detectarEnfermedadIncipiente().foreach { m => if (tgMarcarRecordatorio("ENFERMEDAD")) msgs += m }
        if (esLunes) predecirSobrecargaSemana().foreach { m => if (tgMarcarRecordatorio("SOBRECARGA")) msgs += m }
        if (esLunes && necesitaProtocoloRecuperacion()) generarProtocoloRecuperacion().foreach { texto =>
          if (tgMarcarRecordatorio("PROTOCOLO")) {
            val resumen = texto.linesIterator.map(_.trim).filter(_.nonEmpty).take(2).mkString("\n")
            msgs += s"🔄 PROTOCOLO DE RECUPERACIÓN esta semana:\n$resumen\nVer completo en Guardian."
          }
        }
        if (esLunes && cuenta("SELECT COUNT(*) FROM physical_growth WHERE peso > 0 AND fecha > CURRENT_DATE - 7") == 0 && tgMarcarRecordatorio("PESO"))
          msgs += "⚖️ Sin registro de peso esta semana.\nPESO [kg] o con báscula: PESO [kg] [músculo kg] [masa ósea kg]\nEjemplo: PESO 27.3\nCon báscula: PESO 27.3 19.2 1.1"
      }
      // Diario narrativo del mes anterior (primer lunes de mes, a partir de las 9:00)
      if (hora >= 9 && hora < 22 && esLunes && hoy.getDayOfMonth <= 7) {
        asegurarDiarioMesAnterior().foreach { case (mes, _) =>
          if (tgMarcarRecordatorio(s"DIARIO_$mes", 31)) msgs += s"📖 Diario de ${mesLabel(mes)} generado. Lee el relato del mes en Guardian."
        }
      }
      if (hora == 15) {
        val hayPartido = sesionesHoy.contains("PARTIDO") ||
          cuenta("SELECT COUNT(*) FROM matches WHERE status = 'SCHEDULED' AND fecha = CURRENT_DATE") > 0
        if (hayPartido && !sesionRegistrada(conn, hoy, "PARTIDO") && tgMarcarRecordatorio("PARTIDO"))
          msgs += "🏟️ ¿Cómo fue el partido de hoy?\nPARTIDO [rival] [GF]-[GC] [nota]\nEjemplo: PARTIDO Rivas 2-1 7.5"
      }
      if (hora == 20 && pendiente("ACADEMIA") && tgMarcarRecordatorio("ACADEMIA"))
        msgs += "🎓 ¿Cómo fue la academia de porteros?\nACADEMIA [duración min] [RPE 1-10] [atención 1-5] [calidad 1-5]\nEjemplo: ACADEMIA 60 6 5 4\nO si no fue: ACADEMIA NO [motivo]"
      // BLOQUE H: nuevo record de racha de sueno
      if (hora >= 8 && hora < 22) comprobarRecordStreak().foreach(msgs += _)
      // BLOQUE F: aviso de cambio de fase de Guardian (se comprueba en cada pasada, se envia una vez)
      if (hora >= 8 && hora < 22) comprobarCambioFaseGuardian().foreach(msgs += _)
      // BLOQUE E2: pregunta del RPE de Hector (nunca de noche: entre 22:00 y 8:00 espera a la manana)
      if (hora >= 8 && hora < 22) tgPreguntaRpePendiente(TelegramService.chatIdConfigurado).foreach(msgs += _)
      if (hora == 21) {
        if (pendiente("JUDO") && tgMarcarRecordatorio("JUDO"))
          msgs += "🥋 ¿Fue Héctor a Judo hoy?\nJUDO [duración min] [RPE 1-10]\nEjemplo: JUDO 60 6\nO si no fue: JUDO NO [ENFERMEDAD/FAMILIAR/DESCANSO/OTRO]"
        if (pendiente("EQUIPO") && tgMarcarRecordatorio("CLUB"))
          msgs += "⚽ ¿Cómo fue el entrenamiento de equipo?\nCLUB [duración min] [RPE 1-10] [atención 1-5] [calidad 1-5]\nEjemplo: CLUB 75 7 4 4\nO si no fue: CLUB NO [motivo]"
      }
    } finally { conn.close() }
    msgs.toList
  }

  // ═════════════════════════════════════════════════════════════════════════════
  // BLOQUE E — TIMELINE CRONOLOGICO DE LA CARRERA — SQL puro, sin Gemini
  // ═════════════════════════════════════════════════════════════════════════════
  /** Eventos de todas las fuentes ordenados por fecha. tipo: TODOS | PARTIDO | HITO | LESION | CRECIMIENTO | VOZ_PORTERO. */
  def getCareerTimeline(limit: Int = 50, offset: Int = 0, tipo: String = "TODOS"): List[Map[String, Any]] = {
    val conn = getConnection()
    try {
      val ps = conn.prepareStatement("""
        SELECT * FROM (
          SELECT fecha, 'PARTIDO' as tipo, COALESCE(rival, '') || COALESCE(' ' || goles_favor || '-' || goles_contra, '') as descripcion,
                 CASE WHEN nota > 0 THEN nota::text ELSE '' END as valor, id
          FROM matches WHERE status = 'PLAYED' AND fecha IS NOT NULL
          UNION ALL
          SELECT fecha, 'HITO', descripcion, '', id FROM hitos_conseguidos
          UNION ALL
          SELECT fecha_inicio, 'LESION', COALESCE(NULLIF(tipo, ''), 'Lesión'), COALESCE(zona, ''), id
          FROM injuries WHERE fecha_inicio IS NOT NULL
          UNION ALL
          SELECT fecha, 'CRECIMIENTO', COALESCE(altura::text || ' cm', ''), COALESCE(peso::text || ' kg', ''), id
          FROM physical_growth WHERE fecha IS NOT NULL
          UNION ALL
          SELECT fecha, 'VOZ_PORTERO', motivacion_carita::text, '', id FROM voz_portero
        ) t
        WHERE (? = 'TODOS' OR t.tipo = ?)
        ORDER BY fecha DESC, id DESC LIMIT ? OFFSET ?""")
      ps.setString(1, tipo); ps.setString(2, tipo); ps.setInt(3, limit); ps.setInt(4, offset)
      val rs = ps.executeQuery()
      Iterator.continually(rs).takeWhile(_.next()).map { r =>
        Map[String, Any](
          "fecha" -> r.getDate("fecha").toString,
          "tipo" -> r.getString("tipo"),
          "descripcion" -> fixEncoding(Option(r.getString("descripcion")).getOrElse("")),
          "valor" -> fixEncoding(Option(r.getString("valor")).getOrElse("")),
          "id" -> r.getInt("id"))
      }.toList
    } finally { conn.close() }
  }

}
