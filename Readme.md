# 🛡️ GUARDIAN ELITE v6.0 | Borja Martín R&D Edition

> **"El talento te lleva al área, el carácter te mantiene en la historia."**

**Guardian Elite** es un ecosistema de alto rendimiento diseñado para la monitorización longitudinal (de los 5 a los 20 años) del desarrollo de **Héctor**. Esta plataforma integra Big Data, Machine Learning e IA Generativa para transformar el crecimiento biológico, técnico y cognitivo en un activo estratégico.

![Version](https://img.shields.io/badge/Version-6.0_Cognitive_Data_Ready-white?style=for-the-badge&logo=realmadrid&labelColor=00529F) ![Database](https://img.shields.io/badge/Database-PostgreSQL_Neon-green?style=for-the-badge&logo=postgresql) ![AI](https://img.shields.io/badge/AI-Gemini_2.0_Flash-orange?style=for-the-badge) ![Fase1](https://img.shields.io/badge/Fase_1-COMPLETADA-brightgreen?style=for-the-badge) ![Fase2](https://img.shields.io/badge/Fase_2-COMPLETADA-brightgreen?style=for-the-badge) ![Fase4](https://img.shields.io/badge/Fase_4-COMPLETADA-brightgreen?style=for-the-badge) ![Fase5](https://img.shields.io/badge/Fase_5_Parcial-COMPLETADA-blue?style=for-the-badge)

---

# PARTE I: ECOSISTEMA DESPLEGADO EN PRODUCCION

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
* **Análisis de Goles Encajados:** Registro contextual por gol: origen, situación (1v1/2v1/error defensivo), responsabilidad del portero y zona de portería. Base para PSxG y nota ajustada.
* **Bracket Torneo Visual:** Cuadro de fases interactivo por torneo con resultados y KPIs. URL: `/tournament/bracket`.

### Módulo Médico (Vault)
* **Medical Vault con OCR:** Procesamiento de PDFs médicos con Gemini Vision + extracción de datos estructurados.
* **Historial de lesiones:** Vinculado al calendario de partidos y carga física.

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
* **Mapa de Goles Encajados:** Heatmap de portería 3x3 con intensidad por zona, filtros por temporada y rival. URL: `/mapa-goles`.
* **Laboratorio de Penaltis:** Heatmap de tiros rivales + % parado por zona + historial por rival.
* **Evolución Histórica:** Gráfico de línea de nota media por temporada + barras de goles encajados. URL: `/career/evolucion`.
* **Informe PDF Profesional:** Gráficos embebidos, KPIs globales, tabla de últimos 30 partidos y atributos. URL: `/admin/print_report`.

### Flash-Cards de Decisión
* **Briefing Pre-Partido:** 3 clips recientes del rival + puntos ciegos detectados en biomecánica + estadísticas H2H.
* **Modo Quiz:** Tarjetas de repaso de posicionamiento y decisiones tácticas. URL: `/flash-cards`.

---

## FASE 4 — ML & Estrategia Pro

### Digital Twin | HECTOR 2035
* **Proyección de Altura Adulta:** Algoritmo Tanner midparent `((hPadre + hMadre + 13) / 2)` con ponderación dinámica por edad: 90% genética a los 5 años, 60% datos reales a los 15. Se auto-afina con cada medición registrada en Wellness.
* **PHV Detector:** Detecta automáticamente el Pico de Velocidad de Crecimiento comparando registros consecutivos de altura. Activa alerta PICO ACTIVO si crece >6 cm/año.
* **Inteligencia Deportiva ACWR:** Ratio de carga aguda/crónica con alerta de fatiga, análisis de biotipo (Velocista/Equilibrado/Tanque) y plan de entrenamiento recomendado.
* **Métricas de Portero Proyectadas:** Envergadura adulta, alcance de parada y cobertura de portería (%) calculados desde la altura adulta estimada.
* **Comparativa vs Elite:** Barras comparativas contra la media profesional (189 cm altura / 200 cm envergadura / 251 cm alcance).
* **Informe de Ojeador IA:** Gemini genera 4 bloques: Biotipo / Ventaja Competitiva / Riesgo / Proyección de Nivel.
* **Gráficos:** Curva de crecimiento histórico + proyección hasta los 18 años, y evolución de nota media por temporada.
* **Calibrador Parental:** Inputs de altura padre/madre para recalcular la proyección al instante. URL: `/digital-twin`.

### Biomecánica Posicional
* **Grid 3x3 Interactivo:** Visualización de la portería en 9 zonas con 3 modos: Goles / Paradas / Eficiencia.
* **Detección de Puntos Ciegos:** Zonas donde se encajan más goles que paradas, ordenadas por criticidad.
* **Zonas Fuertes:** Sectores de dominio con barras de progreso comparativas.
* **Tabla de Eficiencia Completa:** Tiros, goles, paradas y % de parada por zona. URL: `/biomecanica`.

---

## FASE 5 — Inteligencia Proactiva

### GK Influence Analytics
* **Score de Influencia 0-100:** Algoritmo ponderado que mide el impacto real de la distribución con el pie en la generación de juego.
* **Gráfico dual:** Barras de acciones con pie + línea de nota de partido (doble eje Y).
* **Control de Saques Estructurados:** % de centros y balones largos completados con éxito.
* **Correlación Influencia-Rendimiento:** Detección automática de si jugar más con el pie mejora la nota final. URL: `/gk-influence`.

### Motor Emocional
* **Score de Resiliencia Mental 0-100:** Calculado a partir del patrón emocional, correlación con rendimiento y estabilidad de ánimo en los últimos 45 días.
* **Análisis Psicopedagógico IA:** Gemini actúa como psicopedagogo deportivo y genera 3 bloques: Patrón Detectado / Fortaleza Mental / Consejo de la Semana.
* **Correlación Ánimo-Nota:** Cuantifica cuántos puntos mejora la nota cuando el ánimo es alto vs. bajo.
* **Detector de Burnout:** Alerta automática si hay rachas de días consecutivos con ánimo/energía bajos.
* **Gráfico Triple 30 días:** Ánimo + Energía (eje izq. 0-5) + Nota de partido (eje der. 0-10).
* **Diario Emocional:** Tabla con las últimas entradas con notas de conducta. URL: `/emocional`.

---

## FASE 6 — Innovación Exclusiva

### Dossier de Captación (Anonimizado)
* **Informe Ciego:** PDF profesional con métricas clave sin datos identificativos, listo para ojeadores. URL: `/scouting-report`.

### Oráculo Biométrico (integrado en Digital Twin)
* **Ratio ACWR:** Monitorización de carga de trabajo aguda/crónica para prevención de lesiones con alertas automáticas.
* **Evolución Biométrica:** Gráfico dinámico Altura vs. Peso con referencia OMS (P15, P50, P85) por edad.
* **Seguimiento de Lesiones:** Registro de zona anatómica, días de baja y curva de recuperación.

---

## FASE 6.5 — Moneyball & Deep Influence Analytics *(Parcial)*

*Scouting cognitivo para encontrar ineficiencias de mercado, valor oculto e impacto estructural del portero*

* **Expected Threat del Portero (xT_GK):** Algoritmo que cuantifica el peligro generado por la distribución con el pie. Pondera eficiencia de pases cortos/largos, volumen y correlación con la nota. ✅ Implementado.
* **Expected Points Saved (xPoints / Clutch Factor):** Ponderación dinámica del valor de cada parada. Mide la relevancia de la intervención multiplicada por la tensión del marcador, tipo de partido y minuto. ✅ Implementado.
* **Sweeper Keeper Index (SPV):** Desglosa el valor de paradas en 1v1 (×1.5), aéreas (×1.2) y normales (×1.0). Score normalizado 0-100. ✅ Implementado.
* **ROI de Entrenamiento:** Correlación de Pearson entre calidad/atención/RPE de sesiones previas y nota en partido. Detecta sobreentrenamiento si correlación RPE es negativa. ✅ Implementado.
* **PSxG — Responsabilidad en Goles Encajados:** Clasifica cada gol como Evitable / Dudoso / Inevitable. Calcula nota ajustada descontando goles por error ajeno. ✅ Implementado.
* **Bypass Rate (Líneas Superadas):** Contabiliza rivales que quedan por detrás tras un pase en salida. Entrada manual en Match Center; schema listo. 🔄 Schema implementado, pendiente UI.
* **Sinergia de Roster (Lineup Value):** Radar de correlación cruzada entre el rendimiento de Héctor y los perfiles de sus compañeros. ❌ Requiere tabla de datos de compañeros.

---

# PARTE II: ROADMAP — LO QUE QUEDA

## FASE 3 — Computer Vision & Video Analysis
*Requiere integración Python/OpenCV — Fuera del stack actual Scala*

* **Pose-Estimation Analyst:** Detección de errores de sustentación y Paso Negativo en video.
* **Goal Coverage Mapping:** Superficie de portería cubierta según biotipo vs. dimensiones reglamentarias.
* **Reaction Time Tracker:** Milisegundos exactos desde el disparo hasta la estirada.

---

## FASE 7 — Career Management Hub
*Infraestructura de análisis profundo para monitorización física, táctica y de entorno a largo plazo*

* **Striker Clustering (ML):** Agrupación de delanteros de la liga en arquetipos vía K-Means. Cruza perfiles con el historial para adaptar el posicionamiento previo.
* **Set-Piece Control (Polígonos de Voronoi):** Cálculo del radio de acción en corners y faltas. Mide el porcentaje de área dominada y el impacto en la anulación de segundas jugadas.
* **Scanning Rate:** Métrica cognitiva que evalúa escaneos de campo antes de recibir una cesión, correlacionando con la tasa de éxito del primer toque bajo presión.
* **Red-Zone Analytics:** Aislamiento estadístico del rendimiento en escenarios de fatiga extrema (últimos 10 minutos) o cuando el rival domina más del 65% de la posesión en el tercio defensivo.
* **Impact Asymmetry Tracker:** Registro de volumen y lateralidad de las caídas cruzado con dureza de superficie. Alerta sobre descompensaciones musculares antes de que deriven en lesiones.
* **HRV Tracker:** Monitorización de variabilidad de frecuencia cardíaca para identificar asimilación de cargas y prevenir sobreentrenamiento.
* **Periodización Nutricional Reactiva:** Ajuste automático de macronutrientes y suplementación cognitiva basado en la carga de estrés táctico y de reflejos de la sesión.
* **Market Estimator (Regresión Lineal):** Compara la curva de evolución de KPIs con bases de datos de élite, proyectando techos de rendimiento y estimaciones de mercado futuras.
* **NLP Scouting Aggregator:** Pipeline de NLP que ingiere informes de ojeadores en texto y extrae sentimiento y palabras clave, transformando opiniones subjetivas en datos estructurados.
* **Dinámicas de Competición Interna:** Análisis de fluctuación de reflejos según nivel de exigencia y rotación de los compañeros de posición en la misma categoría.

---

## FASE 8 — Deep Performance & Cognitive Scouting
*Vanguardia científica para diseccionar técnica pura, biomecánica y robustez psicológica*

* **PSxG Delta (Post-Shot xG vs Goals Conceded):** Métrica definitiva de Shot-Stopping. Evalúa la calidad real del tiro frente a los goles encajados, aislando el mérito individual del rendimiento de la línea defensiva. *(Base implementada en Fase 6.5)*
* **Cognitive Reset Rate:** Aislamiento de los 10 minutos posteriores a un error grave. Cuantifica la capacidad de "reseteo mental" midiendo fluctuaciones en la asunción de riesgos y el tiempo de reacción. *(Datos de contexto de goles ya disponibles)*
* **Set-Stance Timing:** Auditoría por visión artificial del delta de tiempo (en milisegundos) entre el impacto del delantero y el momento en que los pies de Héctor se clavan en el suelo ("set position").
* **Bilateral Power Asymmetry:** Análisis longitudinal de la diferencia de explosividad y alcance entre estiradas izquierda y derecha, para prescribir trabajo compensatorio temprano.
* **Quiet Eye Duration:** Métrica neurocognitiva que cronometra el tiempo de fijación visual en el balón antes del movimiento defensivo, correlacionándolo con el éxito de la estirada.
* **Development Pathway Matcher:** Algoritmo que cruza el estilo de juego de equipos superiores con las necesidades de desarrollo de Héctor, sugiriendo ecosistemas tácticos que aceleren sus áreas de mejora.

---

## FASE 9 — Estructura Profesional & Vanguardia (The Elite Layer)
*Módulos de alta gestión para protección de activos, liderazgo en campo y proyección en ligas profesionales*

* **Vocal Influence Analysis:** Análisis de audio para medir frecuencia, claridad y efectividad de instrucciones tácticas. Cuantifica la capacidad de evitar remates mediante organización vocal de la defensa.
* **Digital Twin Proyectivo (What-if ML):** Modelo virtual basado en el histórico para ejecutar simulaciones sobre cambios en masa muscular, biotipo o contextos de liga específicos.
* **Gaze Behavior Audit:** Análisis de la secuencia de escaneo previa a acciones a balón parado. Detecta patrones de fijación para ampliar la conciencia situacional y evitar el "túnel visual".
* **Guardian Insurance & Contract Vault:** Gestión de cláusulas contractuales, derechos de imagen y optimización de seguros deportivos basados en el historial de lesiones e Índice de Desgaste Articular.
* **Sentiment AI & Media Resilience:** Motor de NLP que monitoriza la narrativa externa en prensa y entornos digitales para preparar la resiliencia mediática ante picos de presión o críticas externas.
* **Neuro-Feedback EEG:** Monitorización de ondas cerebrales en tiempo real. *(Requiere hardware wearable)*
* **Pizarra Táctica AR:** Visualización de posicionamiento en realidad aumentada. *(Requiere app nativa móvil)*
* **Market Value Estimator:** Estimación de valor de mercado con base de datos de traspasos. *(Requiere API externa de canteras)*
* **Entrenamiento VR:** Simulador de escenarios de portería en realidad virtual. *(Requiere dispositivo VR)*
* **Guardian Vault Blockchain:** Certificación inmutable del historial de desarrollo. *(Requiere infraestructura blockchain)*

---

## Stack Tecnológico

| Capa | Tecnología |
|------|-----------|
| Backend | Scala (Cask, Requests) |
| Database | PostgreSQL (Neon Serverless) + HikariCP connection pool |
| AI Engine | Google Gemini 2.0 Flash (v1beta) con caché inteligente |
| Frontend | HTML5, Bootstrap 5, Chart.js |
| PDF/OCR | Gemini Vision para extracción de documentos médicos |
| Hosting | Render.com (Docker) |
| Future Stack | Python (TensorFlow/OpenCV) para Video Analysis y ML |

---

## Estado de Implementación

```
FASE 1 — Core Platform          ██████████ 100%
FASE 2 — Cognitive Synergy      ██████████ 100%
FASE 3 — Computer Vision        ░░░░░░░░░░   0%  (requiere Python/OpenCV)
FASE 4 — ML & Estrategia Pro    ██████████ 100%  (Digital Twin desplegado)
FASE 5 — Inteligencia Proactiva ████████░░  80%  (falta Dojo + Bio-Banding)
FASE 6 — Innovación Exclusiva   ██████████ 100%
FASE 6.5 — Moneyball Analytics  █████░░░░░  55%  (xT_GK, xPoints, SPV, ROI, PSxG implementados)
FASE 7 — Career Management 360  ░░░░░░░░░░   0%  (roadmap definido)
FASE 8 — Deep Performance       ░░░░░░░░░░   0%  (base PSxG + Reset disponible)
FASE 9 — Elite Layer            ░░░░░░░░░░   0%  (requiere infra externa)
```

---

> *"No buscamos porteros que paren. Buscamos atletas que piensen, lideren y dominen."* — **Borja Martin**
