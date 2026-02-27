# 🛡️ GUARDIAN ELITE v6.0 | Borja Martín R&D Edition

> **"El talento te lleva al área, el carácter te mantiene en la historia."**

**Guardian Elite** es un ecosistema de alto rendimiento diseñado para la monitorización longitudinal (de los 5 a los 20 años) del desarrollo de **Héctor**. Esta plataforma integra Big Data, Machine Learning e IA Generativa para transformar el crecimiento biológico, técnico y cognitivo en un activo estratégico.

![Version](https://img.shields.io/badge/Version-6.0_Cognitive_Data_Ready-white?style=for-the-badge&logo=realmadrid&labelColor=00529F) ![Database](https://img.shields.io/badge/Database-PostgreSQL_Neon-green?style=for-the-badge&logo=postgresql) ![AI](https://img.shields.io/badge/AI-Gemini_2.0_Flash-orange?style=for-the-badge) ![Fase1](https://img.shields.io/badge/Fase_1-COMPLETADA-brightgreen?style=for-the-badge) ![Fase2](https://img.shields.io/badge/Fase_2-COMPLETADA-brightgreen?style=for-the-badge) ![Fase4](https://img.shields.io/badge/Fase_4-COMPLETADA-brightgreen?style=for-the-badge) ![Fase5](https://img.shields.io/badge/Fase_5_Parcial-COMPLETADA-blue?style=for-the-badge) ![Fase6](https://img.shields.io/badge/Fase_6-COMPLETADA-brightgreen?style=for-the-badge) ![Fase6.5](https://img.shields.io/badge/Fase_6.5_Parcial-EN_CURSO-yellow?style=for-the-badge)

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
* **Análisis de Goles Encajados:** Registro contextual por gol: origen, situación (1v1/2v1/error defensivo), responsabilidad del portero y zona de portería. Base para PSxG y nota ajustada.
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
* **Mapa de Goles Encajados:** Heatmap de portería 3x3 con intensidad por zona, filtros por temporada y rival. URL: `/mapa-goles`.
* **Laboratorio de Penaltis:** Heatmap de tiros rivales + % parado por zona + historial por rival.
* **Evolución Histórica:** Gráfico de línea de nota media por temporada + barras de goles encajados. URL: `/career/evolucion`.
* **Informe PDF Profesional:** Gráficos embebidos, KPIs globales, tabla de últimos 30 partidos y atributos. URL: `/admin/print_report`.

### Flash-Cards de Decisión
* **Briefing Pre-Partido:** 3 clips recientes del rival + puntos ciegos detectados en biomecánica + estadísticas H2H.
* **Modo Quiz:** Tarjetas de repaso de posicionamiento y decisiones tácticas. URL: `/flash-cards`.

---

## FASE 4 — ML & Estrategia Pro

### Digital Twin | HÉCTOR 2035
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

---

## FASE 6.5 — Moneyball & Deep Influence Analytics *(Parcial)*

*Módulo de scouting cognitivo para encontrar ineficiencias de mercado, valor oculto e impacto estructural del portero.*

* **Expected Threat del Portero (xT_GK):** Algoritmo que cuantifica el peligro generado por la distribución con el pie, ponderando origen y destino del balón. ✅ Implementado.
* **Expected Points Saved (xPoints / Clutch Factor):** Ponderación dinámica del valor de cada parada según la tensión del marcador y el minuto del partido. ✅ Implementado.
* **Sweeper Keeper Index (SPV — Shot Prevention Value):** Desglosa el valor de paradas en 1v1 (×1.5), aéreas (×1.2) y normales (×1.0). Score normalizado 0-100. ✅ Implementado.
* **ROI de Entrenamiento:** Correlación de Pearson entre calidad/atención/RPE de sesiones previas y nota en partido. Detecta sobreentrenamiento si correlación RPE es negativa. ✅ Implementado.
* **PSxG — Responsabilidad en Goles Encajados:** Clasifica cada gol como Evitable / Dudoso / Inevitable. Calcula nota ajustada descontando goles por error ajeno. ✅ Implementado.
* **Bypass Rate (Líneas Superadas):** Contabiliza rivales que quedan por detrás tras un pase en salida, diferenciando distribución libre vs bajo presión. 🔄 Schema implementado, pendiente UI.
* **Sinergia de Roster (Lineup Value):** Radar de correlación cruzada entre el rendimiento de Héctor y los perfiles de sus compañeros: éxito buscando la velocidad de Monje o Kevo, el impacto en Clean Sheet bajo el cerrojo de Dani o la fuerza de Julito, y la fluctuación de métricas ante la capitanía de Luis o la juventud de Beto. ❌ Requiere tabla de datos de compañeros.

---

# PARTE II: ROADMAP — LO QUE QUEDA

## FASE 3 — Computer Vision & Video Analysis
*Requiere integración Python/OpenCV — Fuera del stack actual Scala.*

* **Pose-Estimation Analyst:** Detección de errores de sustentación y Paso Negativo en video.
* **Goal Coverage Mapping:** Superficie de portería cubierta según biotipo vs. dimensiones reglamentarias.
* **Reaction Time Tracker:** Milisegundos exactos desde el disparo hasta la estirada.

---

## FASE 7 — Career Management Hub
*Infraestructura de análisis profundo para monitorización física, táctica y de entorno a largo plazo.*

* **Striker Clustering (ML):** Agrupación de delanteros de la liga en arquetipos vía K-Means para adaptar el posicionamiento previo según el perfil del atacante.
* **Set-Piece Control (Polígonos de Voronoi):** Cálculo del radio de acción en córners y faltas para medir el porcentaje de área dominada de manera efectiva.
* **Scanning Rate:** Métrica cognitiva que evalúa escaneos de campo antes de recibir una cesión, correlacionando con el éxito del primer toque bajo presión.
* **Red-Zone Analytics:** Aislamiento estadístico del rendimiento en escenarios de fatiga extrema (últimos 10 minutos) o asedio rival intenso (>65% posesión).
* **Impact Asymmetry Tracker:** Registro de volumen y lateralidad de las caídas cruzado con la dureza de superficie para alertar sobre descompensaciones musculares.
* **HRV Tracker:** Monitorización de la variabilidad de la frecuencia cardíaca para identificar la asimilación real de cargas de entrenamiento.
* **Periodización Nutricional Reactiva:** Ajuste automático de macronutrientes y suplementación cognitiva basado en el estrés táctico y de reflejos de la sesión.
* **Market Estimator (Regresión Lineal):** Comparativa de evolución de KPIs con bases de datos de élite para proyectar techos de rendimiento y valor de mercado.
* **NLP Scouting Aggregator:** Pipeline que ingiere informes de ojeadores en texto y extrae sentimiento y palabras clave para transformarlos en datos estructurados.

---

## FASE 8 — Deep Performance & Cognitive Scouting
*Vanguardia científica para diseccionar técnica pura, biomecánica y robustez psicológica.*

* **PSxG Delta (Post-Shot xG vs Goals Conceded):** Evaluación de la calidad real del tiro frente a los goles encajados para aislar el mérito individual del portero. *(Base implementada en Fase 6.5)*
* **Cognitive Reset Rate:** Cuantificación de la capacidad de "reseteo mental" tras un error grave midiendo fluctuaciones en la asunción de riesgos y tiempo de reacción. *(Datos de contexto ya disponibles)*
* **Set-Stance Timing:** Auditoría del delta de tiempo entre el impacto del delantero y el momento en que los pies de Héctor se clavan en el suelo ("set position").
* **Bilateral Power Asymmetry:** Análisis de la diferencia de explosividad y alcance entre estiradas izquierda y derecha para prescribir trabajo compensatorio.
* **Quiet Eye Duration:** Métrica neurocognitiva que cronometra el tiempo de fijación visual en el balón antes de iniciar el movimiento defensivo.
* **Development Pathway Matcher:** Algoritmo que sugiere ecosistemas tácticos (equipos superiores) que aceleren las áreas de mejora específicas de Héctor.

---

## FASE 9 — Estructura Profesional & Vanguardia (The Elite Layer)
*Módulos de alta gestión para protección de activos, liderazgo en campo y proyección profesional.*

* **Vocal Influence Analysis:** Análisis de audio para medir frecuencia, claridad y efectividad de instrucciones tácticas y mando de área.
* **Digital Twin Proyectivo (What-if ML):** Modelo virtual para ejecutar simulaciones sobre cambios en masa muscular, biotipo o contextos de liga específicos.
* **Gaze Behavior Audit:** Análisis de la secuencia de escaneo previa a acciones a balón parado para ampliar la conciencia situacional y evitar el "túnel visual".
* **Guardian Insurance & Contract Vault:** Gestión de cláusulas, derechos de imagen y optimización de seguros basados en el historial de lesiones e índice articular.
* **Sentiment AI & Media Resilience:** Monitorización de la narrativa externa en prensa y redes para preparar la resiliencia mediática ante picos de presión.

---

## FASE 10 — Quantum Performance & Science
*Integración de leyes físicas y modelos estocásticos para la precisión absoluta.*

* **Simulador de Trayectorias Magnus (Physics Engine):** Integración de física de fluidos para modelar el vuelo del balón según altitud y presión atmosférica.
* **Markov Career Pathing:** Modelo estocástico para predecir transiciones de categoría y probabilidad de éxito profesional basado en Cadenas de Markov.
* **Vocal Stress Biomarkers:** Análisis de la frecuencia fundamental en el Audio-Diario para detección de picos de cortisol y fatiga suprarrenal antes de síntomas físicos.
* **Tactical Knowledge Graph:** Mapeo de relaciones complejas entre rivales, zonas de riesgo y sinergias defensivas en bases de datos de grafos (Neo4j).
* **Federated Benchmarking:** Sistema de comparación de rendimiento global manteniendo la privacidad absoluta de los datos mediante aprendizaje federado.

---

## FASE 11 — Total Spectrum & Biological Intelligence
*Módulos de ingeniería avanzada para el control de variables internas, biológicas y de equipamiento.*

* **Circadian Performance Index:** Identificación de la "ventana de gloria" biológica según cronotipo para optimizar las horas de carga táctica y física.
* **Análisis de Inferencia Causal:** Evaluación de decisiones mediante modelos contrafácticos para determinar matemáticamente el impacto de decisiones posicionales alternativas.
* **Red de Confianza Táctica (SNA):** Medición de la "química" y centralidad de Héctor con compañeros específicos (seguridad de Dani, destreza de Julito, liderazgo de Luis).
* **Física de Degradación de Materiales:** Cálculo del coeficiente de fricción residual del látex según minutos de uso y clima para alertar sobre riesgo de error técnico.
* **Seguimiento de Carga Cognitiva (Dual-Tasking):** Cuantificación de la caída de precisión en toma de decisiones cuando la frecuencia cardíaca supera el 90%.
* **Modelado de Transferencia Bio-Kinética:** Identificación de fugas de potencia en la cadena cinética de la estirada para optimizar la explosividad muscular.

---

## FASE 12 — Frontiers of Science & Digital Legacy
*Madurez biológica real, inteligencia semántica y soberanía del dato deportivo.*

* **Madurez Ósea Predictiva (Greulich-Pyle AI):** Estimación de la edad ósea real frente a la cronológica para ajustar expectativas de potencia y prevenir sobrecargas.
* **Semantic Tactical Search (RAG):** Implementación de búsqueda semántica sobre el historial completo para consultas en lenguaje natural sobre tendencias de rendimiento.
* **Física de la Barrera y Geometría de Sombra:** Cálculo trigonométrico del posicionamiento óptimo de la barrera para maximizar la cobertura visual de Héctor.
* **Kinetic Signature Analytics:** Identificación del patrón de movimiento único; desviaciones >5% actúan como predictor de riesgo de lesión inminente.
* **Guardian Performance Passport (Blockchain):** Creación de un pasaporte de rendimiento auditable e inmutable que garantiza la soberanía de Héctor sobre sus datos.

---

## FASE 13 — The Biological & Social Deep-Core
*La última frontera: genética, entorno social y ética algorítmica.*

* **Perfilado Nutrigenómico:** Análisis de predisposición genética a lesiones de ligamentos, metabolización de nutrientes y respuesta inflamatoria sistémica.
* **Micro-Ecosistema Social (Invisible Training):** Integración de carga académica, tiempos de viaje y estabilidad del entorno para ajustar el Readiness Score diario.
* **AI Fairness & Evolution Audit:** Sistema de auditoría para asegurar que los modelos de ML no generen sesgos y permitan la evolución natural del atleta.

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
FASE 1  — Core Platform              ██████████ 100%
FASE 2  — Cognitive Synergy          ██████████ 100%
FASE 3  — Computer Vision            ░░░░░░░░░░   0%  (requiere Python/OpenCV)
FASE 4  — ML & Estrategia Pro        ██████████ 100%  (Digital Twin desplegado)
FASE 5  — Inteligencia Proactiva     ████████░░  80%  (falta Dojo + Bio-Banding)
FASE 6  — Innovación Exclusiva       ██████████ 100%
FASE 6.5— Moneyball Analytics        ███████░░░  70%  (xT_GK, xPoints, SPV, ROI, PSxG — falta Bypass UI + Sinergia)
FASE 7  — Career Management 360      ░░░░░░░░░░   0%  (roadmap definido)
FASE 8  — Deep Performance           ░░░░░░░░░░   0%  (base PSxG + Reset disponible)
FASE 9  — Elite Layer                ░░░░░░░░░░   0%  (requiere infra externa)
FASE 10 — Quantum Performance        ░░░░░░░░░░   0%  (modelos estocásticos + física)
FASE 11 — Biological Intelligence    ░░░░░░░░░░   0%  (hardware + sensores)
FASE 12 — Frontiers of Science       ░░░░░░░░░░   0%  (IA avanzada + blockchain)
FASE 13 — Biological & Social Core   ░░░░░░░░░░   0%  (genómica + ética algorítmica)
```

---

> *"No buscamos porteros que paren. Buscamos atletas que piensen, lideren y dominen."* — **Borja Martin**
