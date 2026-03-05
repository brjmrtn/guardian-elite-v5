# 🛡️ GUARDIAN ELITE v7.1 | Borja Martín R&D Edition

> **"El talento te lleva al área, el carácter te mantiene en la historia."**

**Guardian Elite** es un ecosistema de alto rendimiento diseñado para la monitorización longitudinal (de los 5 a los 20 años) del desarrollo de **Héctor**. Esta plataforma integra Big Data, Machine Learning e IA Generativa para transformar el crecimiento biológico, técnico y cognitivo en un activo estratégico.

![Version](https://img.shields.io/badge/Version-7.1_Match_Context-white?style=for-the-badge&logo=realmadrid&labelColor=00529F) ![Database](https://img.shields.io/badge/Database-PostgreSQL_Neon-green?style=for-the-badge&logo=postgresql) ![AI](https://img.shields.io/badge/AI-Gemini_2.0_Flash-orange?style=for-the-badge) ![Fase1](https://img.shields.io/badge/Fase_1-COMPLETADA-brightgreen?style=for-the-badge) ![Fase2](https://img.shields.io/badge/Fase_2-COMPLETADA-brightgreen?style=for-the-badge) ![Fase4](https://img.shields.io/badge/Fase_4-COMPLETADA-brightgreen?style=for-the-badge) ![Fase5](https://img.shields.io/badge/Fase_5-COMPLETADA-brightgreen?style=for-the-badge) ![Fase6](https://img.shields.io/badge/Fase_6-COMPLETADA-brightgreen?style=for-the-badge) ![Fase6.5](https://img.shields.io/badge/Fase_6.5-COMPLETADA-brightgreen?style=for-the-badge) ![Fase7](https://img.shields.io/badge/Fase_7-75%25-yellow?style=for-the-badge) ![Fase8](https://img.shields.io/badge/Fase_8-50%25-yellow?style=for-the-badge)

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

## FASE 7 — Career Management Hub *(75%)*

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
* **Periodización Nutricional Reactiva:** Ajuste de macronutrientes según estrés táctico.
* **Market Estimator (Regresión Lineal):** Proyección de techos de rendimiento.
* **NLP Scouting Aggregator:** Informes de ojeadores en texto → datos estructurados.

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

## Stack Tecnológico

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
FASE 7  — Career Management 360      ███████░░░  75%  (+ Match Context en v7.1)
FASE 8  — Deep Performance           █████░░░░░  50%  (Reset + PSxG + Pathway)
FASE 9  — Elite Layer                ░░░░░░░░░░   0%
FASE 10 — Quantum Performance        ░░░░░░░░░░   0%
FASE 11 — Biological Intelligence    ░░░░░░░░░░   0%
FASE 12 — Frontiers of Science       ░░░░░░░░░░   0%
FASE 13 — Biological & Social Core   ░░░░░░░░░░   0%
```

---

## Rutas Desplegadas en Producción (23 rutas activas)

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
| Cognitive Reset | `/cognitive-reset` | 8 |
| PSxG Delta | `/psxg-delta` | 8 |
| Development Pathway | `/pathway` | 8 |
| Distribution / Oracle | `/distribution` `/oracle` | 6 |

---

> *"No buscamos porteros que paren. Buscamos atletas que piensen, lideren y dominen."* — **Borja Martin**