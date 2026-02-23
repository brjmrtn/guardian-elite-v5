# 🛡️ GUARDIAN ELITE v6.0 | Borja Martín R&D Edition

> **"El talento te lleva al área, el carácter te mantiene en la historia."**

**Guardian Elite** es un ecosistema de alto rendimiento diseñado para la monitorización longitudinal (de los 5 a los 20 años) del desarrollo de **Héctor**. Esta plataforma integra Big Data, Machine Learning e IA Generativa para transformar el crecimiento biológico, técnico y cognitivo en un activo estratégico.

![Version](https://img.shields.io/badge/Version-6.0_Cognitive_Data_Ready-white?style=for-the-badge&logo=realmadrid&labelColor=00529F) ![Database](https://img.shields.io/badge/Database-PostgreSQL_Neon-green?style=for-the-badge&logo=postgresql) ![AI](https://img.shields.io/badge/AI-Gemini_2.0_Flash-orange?style=for-the-badge) ![Fase1](https://img.shields.io/badge/Fase_1-COMPLETADA-brightgreen?style=for-the-badge) ![Fase2](https://img.shields.io/badge/Fase_2-COMPLETADA-brightgreen?style=for-the-badge) ![Fase4](https://img.shields.io/badge/Fase_4_Parcial-COMPLETADA-blue?style=for-the-badge) ![Fase5](https://img.shields.io/badge/Fase_5_Parcial-COMPLETADA-blue?style=for-the-badge)

---

# PARTE I: ECOSISTEMA DESPLEGADO EN PRODUCCION

## 1. DASHBOARD (Inteligencia Central)
* **Carta FUT Dinamica:** Visualizacion gamificada basada en la media ponderada de rendimiento real con algoritmo de Trinquete (Ratchet).
* **IA Neuro-Scout:** Analisis de tendencias con Gemini 2.0 Flash — consejos tecnicos y refuerzo psicologico.
* **Widget Proximo Partido:** Cuenta atras en tiempo real con datos de scouting del rival, historial H2H y estadio.
* **Widget de Alertas:** Panel de alertas inteligentes con deteccion de ACWR en zona de riesgo, rachas sin registro y anomalias de rendimiento.
* **Correlacion Sueno-Rendimiento:** Widget que cruza horas/calidad de sueno con nota del partido siguiente.
* **Grafico RPE por Sesion:** Visualizacion de carga acumulada semanal para detectar sobreentrenamiento.
* **Detector de Fatiga Mental:** Identificacion de periodos de baja concentracion asociados a picos de carga academica.

## 2. MATCH CENTER (Data Ingestion)
* **Modulo Ederson:** Seguimiento de precision en pases cortos y largos (Atributo KIC).
* **Heatmap de Intervenciones:** Registro de coordenadas de paradas y acciones tacticas.
* **Audio-Diario:** Registro de voz y notas de conducta del partido.
* **Bracket Torneo Visual:** Cuadro de fases interactivo por torneo con resultados y KPIs. URL: `/tournament/bracket`.

## 3. EL ORACULO (Biometria y Salud)
* **Ratio ACWR:** Monitorizacion de carga de trabajo aguda/cronica para prevencion de lesiones con alertas automaticas.
* **Evolucion Biometrica:** Grafico dinamico Altura vs. Peso con referencia OMS (P15, P50, P85) por edad.
* **Seguimiento de Lesiones:** Registro de zona anatomica, dias de baja y curva de recuperacion.

## 4. VIDEOTECA
* **Player YouTube:** Playlist automatica de clips etiquetados por tipo (PARADA, GOL, PASE, ERROR) con filtros.
* **Boton MOTIVAME:** Modo automatico que cicla clips con duracion ajustable (10-60s) y bucle infinito.

## 5. ANALITICA AVANZADA
* **Mapa de Goles Encajados:** Heatmap de porteria 3x3 con intensidad por zona, filtros por temporada y rival. URL: `/mapa-goles`.
* **Laboratorio de Penaltis:** Heatmap de tiros rivales + % parado por zona + historial por rival.
* **Evolucion Historica:** Grafico de linea de nota media por temporada + barras de goles encajados. URL: `/career/evolucion`.
* **Informe PDF Profesional:** Graficos embebidos, KPIs globales, tabla de ultimos 30 partidos y atributos. URL: `/admin/print_report`.

## 6. GK INFLUENCE ANALYTICS (Nuevo - Fase 5)
* **Score de Influencia 0-100:** Algoritmo ponderado que mide el impacto real de la distribucion con el pie en la generacion de juego.
* **Grafico dual:** Barras de acciones con pie + linea de nota de partido (doble eje Y).
* **Control de Saques Estructurados:** % de centros y balones largos completados con exito.
* **Correlacion Influencia-Rendimiento:** Deteccion automatica de si jugar mas con el pie mejora la nota final. URL: `/gk-influence`.

## 7. BIOMECANICA POSICIONAL (Nuevo - Fase 4)
* **Grid 3x3 Interactivo:** Visualizacion de la porteria en 9 zonas con 3 modos: Goles / Paradas / Eficiencia.
* **Deteccion de Puntos Ciegos:** Zonas donde se encajan mas goles que paradas, ordenadas por criticidad.
* **Zonas Fuertes:** Sectores de dominio con barras de progreso comparativas.
* **Tabla de Eficiencia Completa:** Tiros, goles, paradas y % de parada por zona. URL: `/biomecanica`.

## 8. MOTOR EMOCIONAL (Nuevo - Fase 5)
* **Score de Resiliencia Mental 0-100:** Calculado a partir del patron emocional, correlacion con rendimiento y estabilidad de animo en los ultimos 45 dias.
* **Analisis Psicopedagogico IA:** Gemini actua como psicopedagogo deportivo y genera 3 bloques: Patron Detectado / Fortaleza Mental / Consejo de la Semana.
* **Correlacion Animo-Nota:** Cuantifica cuantos puntos mejora la nota cuando el animo es alto vs. bajo.
* **Detector de Burnout:** Alerta automatica si hay rachas de dias consecutivos con animo/energia bajos.
* **Grafico Triple 30 dias:** Animo + Energia (eje izq. 0-5) + Nota de partido (eje der. 0-10).
* **Diario Emocional:** Tabla con las ultimas entradas con notas de conducta. URL: `/emocional`.

## 9. MODULO MEDICO (Vault)
* **Medical Vault con OCR:** Procesamiento de PDFs medicos con Gemini Vision + extraccion de datos estructurados.
* **Historial de lesiones:** Vinculado al calendario de partidos y carga fisica.

## 10. MODULO DE GUANTES (Smart Gear)
* **Inventario de Guantes:** Registro de modelos, tipo de latex y condiciones de uso.
* **Recomendacion Meteorologica:** Sugerencia del par optimo cruzando datos del tiempo con el inventario.

## 11. FLASH-CARDS DE DECISION
* **Briefing Pre-Partido:** 3 clips recientes del rival + puntos ciegos detectados en biomecanica + estadisticas H2H.
* **Modo Quiz:** Tarjetas de repaso de posicionamiento y decisiones tacticas. URL: `/flash-cards`.

## 12. DOSSIER DE CAPTACION (Anonimizado)
* **Informe Ciego:** PDF profesional con metricas clave sin datos identificativos, listo para ojeadores. URL: `/scouting-report`.

## 13. CONFIGURACION Y UX
* **Modo Oscuro/Claro:** Toggle persistente en toda la app.
* **Perfil Editable:** Posicion, pie dominante, foto, escudo y fecha de nacimiento editables.
* **Notificaciones Push:** Alertas de partido proximo, recordatorio wellness y avisos ACWR.
* **Predictor de Clean Sheet:** Probabilidad de porteria a cero basada en ACWR, sueno y rival.

---

# PARTE II: ROADMAP — LO QUE QUEDA

## FASE 3: COMPUTER VISION & VIDEO ANALYSIS
*Requiere integracion Python/OpenCV — Fuera del stack actual Scala*

* Pose-Estimation Analyst — deteccion de errores de sustentacion y Paso Negativo en video
* Goal Coverage Mapping — superficie de porteria cubierta segun biotipo vs. dimensiones reglamentarias
* Reaction Time Tracker — milisegundos exactos desde el disparo hasta la estirada
* Analisis de Video Automatico — etiquetado de timestamps (parada, saque, error) con Gemini Vision
* Modo Offline PWA — sincronizacion al recuperar senal

## FASE 5 (Pendiente)

* **Dojo Synergy:** Analisis de como el progreso en Judo impacta en la velocidad de incorporacion tras parada.
* **Bio-Banding Pro:** Monitorizacion del PHV (Peak Height Velocity) para ajustar cargas en picos de crecimiento.

## FASES 7-11: EL OLIMPO
*Requieren infraestructura externa, hardware especializado o APIs de terceros*

| Fase | Feature | Bloqueante |
|------|---------|------------|
| 7 | Digital Twin Proyectivo | Stack ML avanzado |
| 7 | Clutch Factor Analytics | Volumen de datos suficiente |
| 8 | Liderazgo Acustico (Voice Command) | Procesamiento de audio en tiempo real |
| 9 | Deep-Video Highlights IA | Integracion FFmpeg + Gemini Vision |
| 9 | Benchmarking de Elite | API externa de canteras |
| 9 | Bot de Estrategia Personalizado | Fine-tuning sobre historial completo |
| 10 | Neuro-Feedback EEG | Hardware wearable |
| 10 | Pizarra Tactica AR | App nativa movil |
| 11 | Market Value Estimator | Base de datos de traspasos base |
| 11 | Entrenamiento VR | Dispositivo VR + exportacion de datos |
| 11 | Guardian Vault Blockchain | Infraestructura blockchain |

---

## Stack Tecnologico

| Capa | Tecnologia |
|------|-----------|
| Backend | Scala (Cask, Requests) |
| Database | PostgreSQL (Neon Serverless) + HikariCP connection pool |
| AI Engine | Google Gemini 2.0 Flash (v1beta) con cache inteligente |
| Frontend | HTML5, Bootstrap 5, Chart.js |
| PDF/OCR | Gemini Vision para extraccion de documentos medicos |
| Hosting | Render.com (Docker) |
| Future Stack | Python (TensorFlow/OpenCV) para Video Analysis y ML |

---

## Estado de Implementacion

```
FASE 1 — Core Platform          ██████████ 100%
FASE 2 — Cognitive Synergy      ██████████ 100%
FASE 3 — Computer Vision        ░░░░░░░░░░   0%  (requiere Python/OpenCV)
FASE 4 — ML & Estrategia Pro    ████████░░  80%  (falta Digital Twin)
FASE 5 — Inteligencia Proactiva ████████░░  80%  (falta Dojo + Bio-Banding)
FASE 6 — Innovacion Exclusiva   ██████████ 100%
FASES 7-11 — El Olimpo          ░░░░░░░░░░   0%  (out of scope stack actual)
```

---

> *"No buscamos porteros que paren. Buscamos atletas que piensen, lideren y dominen."* — **Borja Martin**
