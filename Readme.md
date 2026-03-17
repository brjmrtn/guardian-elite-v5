# 🛡️ GUARDIAN ELITE v7.3 | Borja Martín R&D Edition

> **"El talento te lleva al área, el carácter te mantiene en la historia."**

**Guardian Elite** es un ecosistema de alto rendimiento diseñado para la monitorización longitudinal (de los 5 a los 20 años) del desarrollo de **Héctor**. Esta plataforma integra Big Data, Machine Learning e IA Generativa para transformar el crecimiento biológico, técnico y cognitivo en un activo estratégico. Incluye **Guardian Amateur**, un sistema paralelo de seguimiento para jugadores de campo en fútbol amateur.

![Version](https://img.shields.io/badge/Version-7.3_Amateur_Season_UX-white?style=for-the-badge&logo=realmadrid&labelColor=00529F) ![Database](https://img.shields.io/badge/Database-PostgreSQL_Neon-green?style=for-the-badge&logo=postgresql) ![AI](https://img.shields.io/badge/AI-Gemini_2.0_Flash-orange?style=for-the-badge) ![Fase1](https://img.shields.io/badge/Fase_1-COMPLETADA-brightgreen?style=for-the-badge) ![Fase2](https://img.shields.io/badge/Fase_2-COMPLETADA-brightgreen?style=for-the-badge) ![Fase4](https://img.shields.io/badge/Fase_4-COMPLETADA-brightgreen?style=for-the-badge) ![Fase5](https://img.shields.io/badge/Fase_5-COMPLETADA-brightgreen?style=for-the-badge) ![Fase6](https://img.shields.io/badge/Fase_6-COMPLETADA-brightgreen?style=for-the-badge) ![Fase6.5](https://img.shields.io/badge/Fase_6.5-COMPLETADA-brightgreen?style=for-the-badge) ![Fase7](https://img.shields.io/badge/Fase_7-95%25-yellow?style=for-the-badge) ![Fase8](https://img.shields.io/badge/Fase_8-50%25-yellow?style=for-the-badge) ![Amateur](https://img.shields.io/badge/Guardian_Amateur-v7.3-blueviolet?style=for-the-badge)

---

# PARTE I: ECOSISTEMA DESPLEGADO EN PRODUCCIÓN

## FASE 1 — Core Platform

### Dashboard (Inteligencia Central)
* **Carta FUT Dinámica:** Visualización gamificada basada en la media ponderada de rendimiento real con algoritmo de Trinquete (Ratchet).
* **IA Neuro-Scout:** Análisis de tendencias con Gemini 2.0 Flash — consejos técnicos y refuerzo psicológico.
* **Widget Próximo Partido:** Cuenta atrás en tiempo real con datos de scouting del rival, historial H2H y estadio.
* **Widget de Alertas:** Panel de alertas inteligentes con detección de ACWR en zona de riesgo, rachas sin registro y anomalías de rendimiento.
* **Correlación Sueño-Rendimiento:** Widget que cruza horas/calidad de sueño con nota del partido siguiente.
* **Gráfico RPE por Sesión:** Visualización de carga acumulada semanal para detectar sobreentrenamiento.
* **Detector de Fatiga Mental:** Identificación de periodos de baja concentración asociados a picos de carga académica.

### Match Center (Data Ingestion)
* **Módulo Ederson:** Seguimiento de precisión en pases cortos y largos (Atributo KIC).
* **Heatmap de Intervenciones:** Registro de coordenadas de paradas y acciones tácticas.
* **Audio-Diario:** Registro de voz y notas de conducta del partido.
* **Análisis de Goles Encajados:** Registro contextual por gol: origen, situación (1v1/2v1/error defensivo), responsabilidad del portero y zona de portería (grid 3×3). Base para PSxG y nota ajustada.
* **Bypass Rate:** Campo de registro de líneas superadas en salida con pie (botones +/−). URL: `/match-center`.
* **Scanning Rate:** Campo de registro de escaneos de campo antes de recibir una cesión (botones +/−). URL: `/match-center`.
* **Local / Visitante:** Selector de tres opciones (sin especificar / 🏠 Local / ✈️ Visitante) que se guarda en la columna `es_local` de la tabla `matches`. Alimenta el análisis de contexto `/match-context`.
* **Bracket Torneo Visual:** Cuadro de fases interactivo por torneo con resultados y KPIs. URL: `/tournament/bracket`.

### Módulo Médico (Vault)
* **Medical Vault con OCR:** Procesamiento de PDFs médicos con Gemini Vision + extracción de datos estructurados.
* **Historial de Lesiones:** Vinculado al calendario de partidos y carga física.

### Módulo de Guantes (Smart Gear)
* **Inventario de Guantes:** Registro de modelos, tipo de látex y condiciones de uso.
* **Recomendación Meteorológica:** Sugerencia del par óptimo cruzando datos del tiempo con el inventario.

### Configuración y UX
* **Modo Oscuro/Claro:** Toggle persistente en toda la app.
* **Perfil Editable:** Posición, pie dominante, foto, escudo y fecha de nacimiento editables.
* **Notificaciones Push:** Alertas de partido próximo, recordatorio wellness y avisos ACWR.
* **Predictor de Clean Sheet:** Probabilidad de portería a cero basada en ACWR, sueño y rival.

---

## FASE 2 — Cognitive Synergy

### Videoteca
* **Player YouTube:** Playlist automática de clips etiquetados por tipo (PARADA, GOL, PASE, ERROR) con filtros.
* **Botón MOTIVAME:** Modo automático que cicla clips con duración ajustable (10-60s) y bucle infinito.

### Analítica Avanzada
* **Mapa de Goles Encajados:** Heatmap de portería 3×3 con intensidad por zona, filtros por temporada y rival. URL: `/mapa-goles`.
* **Laboratorio de Penaltis:** Heatmap de tiros rivales + % parado por zona + historial por rival.
* **Evolución Histórica:** Gráfico de línea de nota media por temporada + barras de goles encajados. URL: `/career/evolucion`.
* **Informe PDF Profesional:** Gráficos embebidos, KPIs globales, tabla de últimos 30 partidos y atributos. URL: `/admin/print_report`.

### Flash-Cards de Decisión
* **Briefing Pre-Partido:** 3 clips recientes del rival + puntos ciegos detectados en biomecánica + estadísticas H2H.
* **Modo Quiz:** Tarjetas de repaso de posicionamiento y decisiones tácticas. URL: `/flash-cards`.

---

## FASE 4 — ML & Estrategia Pro

### Digital Twin | HÉCTOR 2035
* **Proyección de Altura Adulta:** Algoritmo Tanner midparent con ponderación dinámica por edad.
* **PHV Detector:** Detecta automáticamente el Pico de Velocidad de Crecimiento. Activa alerta PICO ACTIVO si crece >6 cm/año.
* **Inteligencia Deportiva ACWR:** Ratio de carga aguda/crónica con alerta de fatiga y plan de entrenamiento.
* **Métricas de Portero Proyectadas:** Envergadura adulta, alcance de parada y cobertura de portería.
* **Comparativa vs Elite:** Barras comparativas contra la media profesional.
* **Informe de Ojeador IA:** Gemini genera 4 bloques: Biotipo / Ventaja Competitiva / Riesgo / Proyección de Nivel.
* **Calibrador Parental:** Inputs de altura padre/madre para recalcular la proyección al instante. URL: `/digital-twin`.

### Biomecánica Posicional
* **Grid 3×3 Interactivo:** Visualización de la portería en 9 zonas con 3 modos: Goles / Paradas / Eficiencia.
* **Detección de Puntos Ciegos:** Zonas donde se encajan más goles que paradas, ordenadas por criticidad.
* **Tabla de Eficiencia Completa:** Tiros, goles, paradas y % de parada por zona. URL: `/biomecanica`.

---

## FASE 5 — Inteligencia Proactiva *(100%)*

### GK Influence Analytics
* **Score de Influencia 0-100** cruzando acciones con pie y generación de juego. URL: `/gk-influence`.

### Motor Emocional
* **Score de Resiliencia Mental 0-100** con análisis psicopedagógico IA, correlación ánimo-nota y detector de burnout. URL: `/emocional`.

### Dojo Cognitivo ✅ Implementado
* **10 situaciones reales de portero** con 3 opciones cada una y puntuación ponderada (0-10 pts por calidad de decisión).
* **Sistema de racha, shuffle aleatorio** y feedback inmediato con explicación táctica de cada respuesta.
* **Clasificación final:** ÉLITE MENTAL / SÓLIDO / EN DESARROLLO / SIGUE ENTRENANDO. Cero dependencia de DB, funciona sin datos. URL: `/dojo`.

### Dojo v2 — Modo Entrenador ✅ Implementado
* **Creador de situaciones personalizadas:** Define situación, contexto, emoji, 3 opciones con puntos y explicación táctica.
* **Sesiones custom reutilizables** con shuffle de opciones, score acumulado y feedback inmediato.
* **El padre o entrenador puede crear situaciones** adaptadas a los fallos específicos de Héctor. URL: `/dojo/entrenador`.

### Bio-Banding ✅ Implementado
* **Fase biológica automática:** INFANCIA TARDÍA / PRE-PUBERTAD / INICIO PUBERTAD / PHV — PICO ACTIVO / POST-PHV / MADUREZ, calculada desde la fecha de nacimiento y el PHV detector del Digital Twin.
* **Factor de ajuste de nota:** Durante PHV activo, una nota real se multiplica ×1.15 — un 6.5 real equivale a un 7.5 en condiciones normales, porque el cuerpo consume energía en crecer.
* **Nota real vs nota bio-ajustada** en todos los partidos con gráfico comparativo y tabla individual.
* **Percentil de altura OMS** para la edad. URL: `/bio-banding`.

---

## FASE 6 — Innovación Exclusiva *(100%)*

### Dossier de Captación (Anonimizado)
* **Informe Ciego:** PDF profesional con métricas clave sin datos identificativos, listo para ojeadores.

---

## FASE 6.5 — Moneyball & Deep Influence Analytics *(100%)* ✅

* **xT_GK** — Expected Threat del portero en distribución con el pie. ✅
* **xPoints / Clutch Factor** — Valor de paradas según tensión del marcador y minuto. ✅
* **SPV** — Sweeper Keeper Index: 1v1 ×1.5, aéreas ×1.2, normales ×1.0. Score 0-100. ✅
* **ROI de Entrenamiento** — Correlación Pearson entre calidad/atención/RPE y nota partido. ✅
* **PSxG** — Nota ajustada descontando goles por error ajeno. ✅
* **Bypass Rate** — Registro en Match Center + visualización en Moneyball. ✅
* **Bypass Rate Histórico** — Gráfico de evolución por temporada: barras (líneas/partido) + línea (eficiencia %). Tabla con tendencia ↑/↓/→ año a año. ✅ *(nuevo en v7.1)*
* **Sinergia de Roster** — ❌ Descartado: rotación excesiva a esta edad, datos insuficientes.

URL: `/moneyball`.

---

## FASE 7 — Career Management Hub *(95%)*

### Red-Zone Analytics ✅ Implementado
* **Resilience Index 0-100:** Rendimiento bajo asedio (GC ≥ 2) vs media global. ÉLITE / SÓLIDO / EN PROCESO / VULNERABLE.
* **Fatigue Index 0-100:** Rendimiento en partidos largos (≥ 70 min). SIN CAÍDA / AGUANTA / LEVE CAÍDA / FATIGA CLARA.
* **Colapso Total (GC ≥ 3):** Aislamiento estadístico de los peores partidos.
* **Tabla y gráfico** de episodios con evolución temporal. URL: `/red-zone`.

### Striker Clustering ✅ Implementado
* **Clasificación automática de rivales** en 5 arquetipos: RÁPIDO (1v1) / AÉREO (cabezazos) / COLECTIVO (2v1) / DIRECTO (alto GC) / EQUILIBRADO.
* **Alerta del arquetipo más frecuente** con consejo táctico específico.
* **Directorio de rivales** con nota media de Héctor, amenaza y desglose. URL: `/striker-clustering`.

### Scanning Rate ✅ Implementado
* **Campo nuevo en Match Center** (botones +/−) para registrar escaneos de campo antes de recibir cesión.
* **Correlación de Pearson** automática entre escaneos y nota del partido.
* **Gráfico dual** barras/línea con doble eje Y. URL: `/scanning-rate`.

### Match Context Analytics ✅ Implementado *(nuevo en v7.1)*
* **Por tipo de partido:** Nota media, GC y limpias en LIGA vs TORNEO vs AMISTOSO. Columna "vs media global" con badge verde/rojo.
* **Por clima:** Rendimiento cruzado con la condición meteorológica registrada en cada partido.
* **Local vs Visitante:** Selector explícito en el Match Center (`es_local BOOLEAN`). Dos cajas comparativas con nota, GC, limpias y conclusión automática cuando hay ≥2 partidos en cada categoría.
* **Por duración:** Franjas de minutos (<40 / 40-59 / 60-79 / 80+) para detectar si los partidos completos benefician o perjudican el rendimiento.
* **Tendencia mensual:** Gráfico línea + barras de los últimos 12 meses con línea de media global de referencia.
* Sin nuevos campos requeridos — usa datos ya registrados en el Match Center. URL: `/match-context`.

### Market Estimator ✅ Implementado *(nuevo en v7.2)*
* **Regresión lineal multivariable** ponderada sobre 5 dimensiones: nota media (35%), SPV (20%), bypass rate eficiencia (15%), PSxG delta (15%), win rate (10%) + factor bio-banding (5%).
* **Valor de mercado formativo estimado en €** con escala calibrada para porteros de academia sub-14 a sub-17.
* **Percentil vs academias españolas** por grupo de edad: tabla de referencia P10/P25/P50/P75/P90.
* **Niveles formativos:** EN DESARROLLO / FORMATIVO MEDIO / ACADEMIA REGIONAL / ACADEMIA PRIMERA / ELITE NACIONAL.
* **Informe de captación IA** con diagnóstico, palancas de valor y ruta al siguiente nivel.
* **Evolución del score por temporada** con gráfico de línea. URL: `/market-estimator`.

### NLP Scouting Aggregator ✅ Implementado *(nuevo en v7.2)*
* **Formulario de ingesta de texto libre:** Pega cualquier informe de ojeador en lenguaje natural.
* **Gemini extrae automáticamente:** Valoración 0-10 por dimensión (técnica, táctica, física, mental, distribución), nivel global, proyección (ELITE / PRIMERA / SEGUNDA / REGIONAL / FORMATIVO) y recomendación (FICHAR YA / SEGUIMIENTO 6M / SEGUIMIENTO 12M / DESCARTAR).
* **Fortalezas y áreas de mejora** estructuradas + resumen ejecutivo de 3-4 frases.
* **Historial persistente** de todos los informes con radar de 5 atributos, badge de proyección y recomendación color-coded.
* **Tabla `scouting_reports`** nueva en DB (texto raw + todos los campos estructurados). URL: `/scouting/nlp`.

### Periodización Nutricional Reactiva ✅ Implementado *(nuevo en v7.2)*
* **Contexto reactivo:** Lee ACWR actual, RPE media de los últimos 7 días, nota del último partido, próximo partido y datos físicos (altura/peso).
* **Gemini genera un plan semanal completo** con macros diarios (proteína, carbohidratos, grasas, hidratación), distribución por tipo de día (pre-partido / partido / recuperación), alimentos clave y alerta nutricional específica.
* **Cache de 6 días:** El plan no se regenera innecesariamente — botón "Regenerar" fuerza actualización.
* **Tabla `nutrition_plans`** nueva en DB. URL: `/nutrition`.

---

## FASE 8 — Deep Performance & Cognitive Scouting *(50%)*

### Cognitive Reset Rate ✅ Implementado
* **Reset Score 0-100:** % de veces que Héctor recupera nivel en el partido siguiente a uno con gol evitable.
* **REBOTE / ESTABLE / IMPACTO** con gráfico y tabla de episodios. URL: `/cognitive-reset`.

### PSxG Delta ✅ Implementado
* **xGBase calibrada:** 9 zonas × multiplicadores de situación (penalti ×1.60, 1v1 ×1.35, libre ×0.85).
* **Delta real vs esperado**, desglose por dificultad y grid 3×3 color-coded. URL: `/psxg-delta`.

### Development Pathway Matcher ✅ Implementado
* **Cruza el arquetipo de rivales con el rendimiento real** de Héctor para detectar contra qué estilo de equipo crece más como portero.
* **Entorno de máximo crecimiento** y **área de mejora prioritaria** con recomendación táctica concreta.
* **Tabla comparativa** por arquetipo: nota media, paradas/partido, pie/partido, bypass/partido. URL: `/pathway`.

---

# PARTE II: ROADMAP — LO QUE QUEDA

## FASE 3 — Computer Vision & Video Analysis
*Requiere integración Python/OpenCV — Fuera del stack actual Scala.*

* **Pose-Estimation Analyst:** Detección de errores de sustentación y Paso Negativo en video.
* **Goal Coverage Mapping:** Superficie de portería cubierta según biotipo vs. dimensiones reglamentarias.
* **Reaction Time Tracker:** Milisegundos exactos desde el disparo hasta la estirada.

---

## FASE 7 — Career Management Hub *(Roadmap)*

* **Set-Piece Control (Polígonos de Voronoi):** Radio de acción en córners y faltas.
* **Impact Asymmetry Tracker:** Lateralidad de caídas cruzada con dureza de superficie.
* **HRV Tracker:** Variabilidad de frecuencia cardíaca (requiere wearable).
* ~~**Periodización Nutricional Reactiva:** Ajuste de macronutrientes según estrés táctico.~~ ✅ **Implementado en v7.2** — `/nutrition`
* ~~**Market Estimator (Regresión Lineal):** Proyección de techos de rendimiento.~~ ✅ **Implementado en v7.2** — `/market-estimator`
* ~~**NLP Scouting Aggregator:** Informes de ojeadores en texto → datos estructurados.~~ ✅ **Implementado en v7.2** — `/scouting/nlp`

---

## FASE 8 — Deep Performance *(Roadmap)*

* **Set-Stance Timing:** Delta entre impacto del delantero y posición de set de Héctor.
* **Bilateral Power Asymmetry:** Diferencia de explosividad entre estiradas izquierda y derecha.
* **Quiet Eye Duration:** Tiempo de fijación visual en el balón antes del movimiento defensivo.

---

## FASE 9 — Estructura Profesional & Vanguardia

* **Vocal Influence Analysis:** Frecuencia, claridad y efectividad de instrucciones tácticas.
* **Digital Twin Proyectivo (What-if ML):** Simulaciones sobre cambios en masa muscular o contextos de liga.
* **Gaze Behavior Audit:** Secuencia de escaneo previa a balón parado.
* **Guardian Insurance & Contract Vault:** Cláusulas, derechos de imagen y seguros.

---

## FASE 10 — Quantum Performance & Science

* **Simulador de Trayectorias Magnus:** Física de fluidos para modelar el vuelo del balón.
* **Markov Career Pathing:** Cadenas de Markov para predecir transiciones de categoría.
* **Vocal Stress Biomarkers:** Frecuencia fundamental del Audio-Diario para detección de cortisol.
* **Tactical Knowledge Graph:** Grafos de relaciones entre rivales, zonas y sinergias.

---

## FASE 11-13 — Fronteras de la Ciencia

* Circadian Performance Index, inferencia causal, física de materiales, madurez ósea IA, Blockchain Passport, nutrigenómica, auditoría de fairness algorítmico.

---

# PARTE III: GUARDIAN AMATEUR — Sistema Paralelo

**Guardian Amateur** es un sistema de seguimiento integrado dentro de la misma plataforma, pensado para jugadores de campo en fútbol amateur y semiprofesional. Comparte la infraestructura de autenticación con Guardian Elite pero opera con tablas de datos, métricas y lógica completamente independientes.

## Arquitectura y Autenticación

* **Autenticación unificada** con Guardian Elite: una sola cookie `guardian_session`, un solo login. Al autenticarse, la plataforma detecta el tipo de perfil (Elite o Amateur) y redirige al dashboard correspondiente.
* **Profile Switcher:** Los usuarios con acceso a ambos sistemas pueden cambiar entre perfiles desde el header sin volver a autenticarse.
* **Controladores independientes:** `AmateurController.scala` (1.600+ líneas) + `AmateurDatabaseManager.scala` (850+ líneas).

## Base de Datos Amateur (5 tablas)

| Tabla | Descripción |
|-------|-------------|
| `am_users` | Usuarios del sistema Amateur con datos de perfil (nombre, equipo, posición, dorsal, foto) |
| `am_matches` | Partidos registrados con nota, goles favor/contra, posición jugada (portero/jugador de campo), goles marcados, asistencias |
| `am_penalties` | Historial de penaltis (para jugadores que los lanzan) |
| `am_gear` | Inventario de equipamiento (botas, guantes si porta, etc.) |
| `am_calendar` | Agenda de próximos partidos con tipo (LIGA / TORNEO / CUP / AMISTOSO) y datos del rival |
| `am_seasons` | Resumen archivado de temporadas cerradas: PJ, G/E/P, nota media, GC media, limpias |

## Funcionalidades *(v7.3 — 100%)*

### Dashboard Amateur
* **Carta FUT Amateur** con nota media y estadísticas globales.
* **Widget Próximo Partido** con cuenta atrás en tiempo real (JS countdown: "Xd Xh para el partido" / "HOY JUEGAS!"), badge de tipo de partido color-coded y botones de acción directos.
* **Desglose portero/jugador de campo** según la posición registrada en cada partido.
* **Estadísticas globales:** nota media, GC/partido, porterías a cero (%), partidos ganados/empatados/perdidos.
* Último 5 partidos con badge de posición (PORTERO / JUGADOR).
* **Win rate %** con código de color (verde ≥60%, amarillo ≥40%, rojo <40%).
* **Sistema de temporadas:** indicador de temporada activa + botón "🏁 Finalizar temporada" con modal de confirmación + historial de temporadas anteriores (G/E/P + nota media).

### Match Center Amateur
* **Selector de posición por partido:** Dos botones — Portero (oculta goles/asistencias, muestra goles encajados) y Jugador de campo (muestra posición específica: Delantero / Centrocampista / Extremo / Defensa + goles marcados + asistencias).
* Registro completo: rival, resultado, nota 0-100, minutos, tipo de partido, clima, estadio, notas.
* **Sección de goles encajados oculta automáticamente** cuando se selecciona "Jugador de campo" — solo visible en modo portero.

### Agenda / Calendario
* **Vista mensual** (grid) y lista de próximos partidos.
* Añadir, editar y eliminar entradas de agenda con tipo de partido, rival, fecha y hora.
* Alimenta el widget de próximo partido del dashboard.

### Progresión y Tendencias *(nuevo en v7.3)*
* **Tendencia principal:** compara la media de los últimos 5 partidos vs los 5 anteriores → ↑ Mejorando / → Estable / ↓ Bajando.
* **Forma reciente:** últimos 10 resultados como píldoras G/E/P con código de color.
* **Gráfico de evolución de nota:** Chart.js línea cronológica de todos los partidos.
* **Gráfico de goles encajados:** barras verdes (0 GC) / amarillo (1) / rojo (2+).
* **Tabla por mes:** nota media, PJ, ganados, limpias — últimos 6 meses.
* **Mejor y peor partido:** tarjetas con rival, resultado y nota. URL: `/am/progression`.

### Historial Amateur
* Tabla de todos los partidos con filtros, badges de posición y exportación.

### Penaltis Amateur
* Heatmap de lanzamientos propios (para delanteros/centrocampistas que lanzan penaltis).

### Informe PDF Amateur
* Exportación A4 print-optimized con: cabecera, próximo partido, estadísticas globales, desglose posición y tabla de los últimos 20 partidos. URL: `/am/report`.

## Rutas Amateur (17 rutas activas)

| Ruta | Método | Descripción |
|------|--------|-------------|
| `/am` | GET | Dashboard principal |
| `/am/match` | GET | Match Center — registrar partido |
| `/am/match/save` | POST | Guardar partido |
| `/am/history` | GET | Historial de partidos |
| `/am/penalties` | GET | Módulo de penaltis |
| `/am/penalties/save` | POST | Guardar penalti |
| `/am/gear` | GET | Inventario de equipamiento |
| `/am/gear/add` | POST | Añadir equipamiento |
| `/am/calendar` | GET | Agenda mensual |
| `/am/calendar/add` | GET | Formulario añadir a agenda |
| `/am/calendar/save` | POST | Guardar entrada de agenda |
| `/am/calendar/delete` | POST | Eliminar entrada de agenda |
| `/am/report` | GET | Exportar informe PDF (print HTML) |
| `/am/progression` | GET | Progresión y tendencias con gráficos |
| `/am/end-season` | GET | Finalizar temporada activa y archivar resumen |
| `/am/logout` | GET | Cerrar sesión (limpia cookie, redirige a selector de perfiles) |

## ¿Tendrá el Amateur acceso a módulos Elite?

En la versión actual, Guardian Amateur opera como sistema standalone. Las siguientes integraciones están planificadas para versiones futuras:

* **Bio-Banding simplificado:** Factor de madurez para rendimiento en contexto formativo.
* **Moneyball Amateur:** xGoals_scored, pases clave, participación en goles (goals + assists / partidos).
* **Digital Twin Amateur:** Proyección de altura adulta (usa los mismos datos biométricos de `am_users`).

---

| Capa | Tecnología |
|------|-----------|
| Backend | Scala (Cask, Requests) |
| Database | PostgreSQL (Neon Serverless) + HikariCP |
| AI Engine | Google Gemini 2.0 Flash (v1beta) con caché inteligente |
| Frontend | HTML5, Bootstrap 5, Chart.js |
| PDF/OCR | Gemini Vision para documentos médicos |
| Hosting | Render.com (Docker) |
| Future Stack | Python (TensorFlow/OpenCV) para Video Analysis y ML |

---

## Estado de Implementación

```
FASE 1  — Core Platform              ██████████ 100%
FASE 2  — Cognitive Synergy          ██████████ 100%
FASE 3  — Computer Vision            ░░░░░░░░░░   0%  (requiere Python/OpenCV)
FASE 4  — ML & Estrategia Pro        ██████████ 100%
FASE 5  — Inteligencia Proactiva     ██████████ 100%  (Dojo + Dojo Entrenador + Bio-Banding)
FASE 6  — Innovación Exclusiva       ██████████ 100%
FASE 6.5— Moneyball Analytics        ██████████ 100%  (Bypass Rate histórico completado)
FASE 7  — Career Management 360      █████████░  95%  (+ Market Estimator + NLP Scouting + Nutrición en v7.2)
FASE 8  — Deep Performance           █████░░░░░  50%  (Reset + PSxG + Pathway)
FASE 9  — Elite Layer                ░░░░░░░░░░   0%
FASE 10 — Quantum Performance        ░░░░░░░░░░   0%
FASE 11 — Biological Intelligence    ░░░░░░░░░░   0%
FASE 12 — Frontiers of Science       ░░░░░░░░░░   0%
FASE 13 — Biological & Social Core   ░░░░░░░░░░   0%
GUARDIAN AMATEUR                     ██████████ 100%  (v7.3: temporadas, progresión, toggle portero/jugador, UX fixes)
```

---

## Rutas Desplegadas en Producción — Guardian Elite (26 rutas activas)

| Módulo | URL | Fase |
|--------|-----|------|
| Dashboard | `/` | 1 |
| Match Center | `/match-center` | 1 |
| Historial | `/history` | 1 |
| Lesiones | `/lesiones` | 1 |
| Flash-Cards | `/flash-cards` | 2 |
| Mapa de Goles | `/mapa-goles` | 2 |
| Biomecánica | `/biomecanica` | 4 |
| Digital Twin | `/digital-twin` | 4 |
| GK Influence | `/gk-influence` | 5 |
| Motor Emocional | `/emocional` | 5 |
| Dojo Cognitivo | `/dojo` | 5 |
| Dojo Entrenador | `/dojo/entrenador` | 5 |
| Bio-Banding | `/bio-banding` | 5 |
| Scouting | `/scouting` | 6 |
| Moneyball | `/moneyball` | 6.5 |
| Red-Zone | `/red-zone` | 7 |
| Striker Clustering | `/striker-clustering` | 7 |
| Scanning Rate | `/scanning-rate` | 7 |
| Match Context | `/match-context` | 7 |
| **Market Estimator** | **`/market-estimator`** | **7 — nuevo v7.2** |
| **NLP Scouting** | **`/scouting/nlp`** | **7 — nuevo v7.2** |
| **Nutrición Reactiva** | **`/nutrition`** | **7 — nuevo v7.2** |
| **Progresión Amateur** | **`/am/progression`** | **Amateur — nuevo v7.3** |
| **Sistema Temporadas** | **`/am/end-season`** | **Amateur — nuevo v7.3** |
| Cognitive Reset | `/cognitive-reset` | 8 |
| PSxG Delta | `/psxg-delta` | 8 |
| Development Pathway | `/pathway` | 8 |
| Distribution / Oracle | `/distribution` `/oracle` | 6 |

---

---

## Changelog v7.3 *(17 Mar 2026)*

### Guardian Amateur
* **Sistema de temporadas completo:** nueva tabla `am_seasons`, columna `current_season_num` en `am_matches`, método `endSeason()` que archiva resumen estadístico y bumps el contador. Dashboard muestra temporada activa + historial de temporadas anteriores con G/E/P y nota media.
* **Progresión y Tendencias** (`/am/progression`): página nueva con tendencia principal, forma reciente (últimos 10), gráfico de nota por Chart.js, gráfico de GC, tabla mensual y cards de mejor/peor partido.
* **Toggle portero / jugador de campo mejorado:** `togglePosicion()` ahora oculta correctamente la sección "Goles encajados" cuando se juega de jugador de campo y la restaura al volver a portero.
* **Win rate % en dashboard** con código de color dinámico.
* **Emojis del nav corregidos:** se reemplazaron entidades HTML escapadas (`&#127968;` etc.) por caracteres UTF-8 directos para compatibilidad con ScalaTags.
* **Fix ruta duplicada** `/profiles`: la ruta de logout de Amateur renombrada a `/am/logout`.
* **Fix `logMatch`:** coma faltante en la firma del método que impedía compilar `getReportData` y `getProgressionData`.
* **Fix `AmMatch`:** constructores en `getMatches` y `getMatch` actualizados con los 4 campos de v7.2 (`posicionPartido`, `posicionCampo`, `golesMarcados`, `asistencias`).

### Fixes de compilación
* `AmateurController:1539` — `mkString("↵")` con salto de línea literal → `mkString("\n")`.
* `HistoryController:4506` — `s"""` en bloque JS sin interpolaciones → `"""` (sin prefijo `s`).
* `AM_COOKIE` not found en rutas cask — inlineado como literal `"guardian_session"`.

---

> *"No buscamos porteros que paren. Buscamos atletas que piensen, lideren y dominen."* — **Borja Martin**