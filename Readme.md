# 🛡️ GUARDIAN ELITE v4.0 | High Performance Center

> **"El talento te lleva al área, el dato te hace dueño de la portería."**

**Guardian Elite** no es una simple app de estadísticas. Es un ecosistema integral de gestión deportiva diseñado para monitorizar, analizar y potenciar la carrera de **Héctor**. Combina la analítica de datos profesional (Big Data), la gamificación (estilo FUT) y la Inteligencia Artificial Generativa (Gemini AI) para maximizar el desarrollo técnico, táctico, físico y mental.

![Version](https://img.shields.io/badge/Version-4.0_Fútbol_Total-blue?style=for-the-badge&logo=scala) ![Database](https://img.shields.io/badge/Database-PostgreSQL_Neon-green?style=for-the-badge&logo=postgresql) ![AI](https://img.shields.io/badge/AI-Gemini_Powered-orange?style=for-the-badge&logo=google-gemini)

---

## 🗺️ Mapa del Territorio

La aplicación se estructura en **7 Módulos de Operaciones**, accesibles desde la barra de navegación inferior:

1.  **🏠 Dashboard:** Inteligencia y Estado Actual.
2.  **⚽ Match Center:** Registro de Partidos (Live/Post).
3.  **🧬 Bio & Entreno:** Fisiología, Cargas y Entrenador IA.
4.  **🧤 Garage:** Gestión de Material (Botas/Guantes).
5.  **📋 Pizarra Táctica:** The Locker Room.
6.  **🕵️ Scouting:** Inteligencia de Rivales (Black Book).
7.  **🥅 Penalty Lab:** Laboratorio de Penaltis.

---

## 📖 MANUAL DE USO: Módulo a Módulo

### 1. 🏠 DASHBOARD (Centro de Mando)
*La primera pantalla que ves. Aquí se toman las decisiones.*

#### 👤 La Carta FUT (Player Card)
Visualización gamificada del nivel actual.
* **Media Global:** Calculada con el **Algoritmo de Trinquete**. La media nunca baja; si tiene un mal partido, se estanca, pero no retrocede. Esto protege la moral del jugador.
* **Barra de XP:** Justo debajo de la carta. Muestra el porcentaje decimal (ej: 45%) para subir al siguiente punto de media.
* **Stats (DIV, HAN, KIC...):** Se actualizan automáticamente tras cada partido según el rendimiento.

#### 🧠 IA Neuro-Scout (El Segundo Entrenador)
Es el recuadro azul de alerta. **Gemini AI** analiza en tiempo real:
1.  **Bio-Ritmo:** ¿Durmió bien hoy? ¿Está cansado?
2.  **Crecimiento:** ¿Ha dado un estirón reciente (>0.5cm/mes)? (Alerta de torpeza motora).
3.  **Racha:** ¿Cómo le fue en los últimos 3 partidos?
4.  **Agenda:** ¿Quién es el próximo rival?
* **Resultado:** Te da un consejo táctico/físico específico para HOY.

#### 📊 Gráficas de Rendimiento
* **Radar Chart:** Comparativa visual de las 6 estadísticas clave.
* **Growth Chart:** Evolución de la media a lo largo de la temporada.
* **Donde te marcan / Donde paras:** Mapa de calor estadístico de la portería.

---

### 2. ⚽ MATCH CENTER (El Partido)
*Donde ocurre la verdad. Diseñado para usarse en el descanso o al llegar a casa.*

#### 📝 Configuración Inicial
* **Rival:** Nombre del equipo.
* **Fecha:** Automática (hoy) o seleccionable.
* **Marcador:** `GF` (Goles Favor) y `GC` (Goles Contra).
* **Paradas:** Contador total (se actualiza solo al usar la rejilla).

#### 🎯 El "Módulo Ederson" (Distribución)
Control obsesivo del juego de pies.
* **Corto:** Toca `✅` si el pase al defensa fue bueno, `❌` si falló.
* **Largo:** Toca `✅` si el desplazamiento superó líneas y llegó al compañero.
* *Nota:* Esto afecta directamente al atributo **KIC**.

#### 🥅 Rejilla Interactiva (3x3)
La portería se divide en 9 cuadrantes (TL, TM, TR, ML, MM, MR, BL, BM, BR).
1.  **Modo Parada (Botón Verde):** Toca la zona donde detuvo el balón.
2.  **Modo Gol (Botón Rojo):** Toca la zona por donde entró el gol.

#### ⚡ Acciones Específicas
* **1vs1:** Duelos ganados al delantero.
* **Aéreo:** Balones descolgados o despejados por alto.
* **Pie:** Acciones de corte fuera del área o regates (líbero).

#### 📹 Sala de Video & Entorno
* **Clima:** Selecciona las condiciones (Lluvia, Sol...). Afecta al atributo **HAN** (Handling).
* **Link Video:** Pega aquí el enlace de YouTube o Drive del partido.

#### 🧠 Análisis Técnico
* **Notas:** Observaciones generales (liderazgo, colocación...).
* **Reacción/Goles:** ¿Cómo reaccionó mentalmente tras un error o gol? Vital para la evolución psicológica.
* **Nota (0-10):** Tu valoración subjetiva.

---

### 3. 🧬 BIO & ENTRENO (El Motor)
*El cuerpo y la mente del atleta.*

#### 🩺 Wellness (Diario)
Rellenar por la mañana para calibrar la IA.
* **Estado Físico:** Disponible, Molestias, Lesión.
* **Sueño:** Calidad (1-5) y Horas. Fundamental para la recuperación.
* **Energía/Ánimo:** Sliders del 1 al 5.
* **Crecimiento:** Introduce Altura (cm) y Peso (kg). El sistema calcula la "Velocidad de Crecimiento".

#### 🏋️ Registro de Entreno (Híbrido)
* **Tipo:**
  * *Club / Academia:* Solo registra carga (RPE) y calidad.
  * *Papá (Portero) / Papá (Jugador):* Habilita el diseñador de sesiones.
* **Diseñador con IA:**
  1.  Escribe un objetivo en el campo "Foco" (ej: "Salida de balón", "Reflejos", "Desmarques").
  2.  Pulsa el botón **🤖 IA**.
  3.  El sistema generará una rutina de 45 min adaptada al rol (Portero o Jugador) y basada en los errores del último partido.

#### 🎯 Misiones Activas
Barra de progreso de objetivos a corto plazo (ej: "Completar 10 sesiones de blocaje"). Se llenan solas al registrar entrenos "Papá".

---

### 4. 🕵️ SCOUTING (Black Book)
*La inteligencia competitiva.*

* **Buscador:** Escribe el nombre del equipo rival.
* **Ficha del Rival:**
  * **Estilo:** ¿Juegan directo o combinativo?
  * **Claves:** "El 9 es zurdo y rápido", "El portero duda en salidas".
  * **Notas:** Historial de observaciones.
* **Historial vs Rival:** Muestra automáticamente todos los partidos jugados contra ellos, con resultados y links de video.

---

### 5. 🧤 GARAGE (Gestión de Material)
*Enseña al profesionalismo cuidando las herramientas.*

* **Añadir Material:** Sube una **FOTO REAL** de los guantes o botas.
* **Barra de Vida:** Define una vida útil (ej: 30 usos). La barra baja automáticamente cada vez que registras un partido o entrenamiento.
* **Semáforo:**
  * 🟢 Verde: Nuevo/Buen estado.
  * 🟡 Amarillo: Desgaste visible.
  * 🔴 Rojo: Crítico/Cambiar (Alerta visual).

---

### 6. 📋 PIZARRA TÁCTICA (The Locker Room)
*Para explicaciones rápidas en el descanso o en el coche.*

* **Canvas Interactivo:** Un campo de fútbol verde proporcional al móvil.
* **Lápices:**
  * ⚪ **Blanco:** Balón y movimientos generales.
  * 🟡 **Amarillo:** Héctor / Portero.
  * 🔴 **Rojo:** Rival / Peligro.
  * 🔵 **Azul:** Equipo propio / Defensas.
* **Multitouch:** Funciona con el dedo.
* **Borrar:** Limpia la pizarra instantáneamente.

---

### 7. 🥅 PENALTY LAB (Laboratorio)
*Análisis específico de la pena máxima.*

* **Registrar:**
  * **Rival:** (Opcional).
  * **Zona Tiro:** ¿Dónde fue el balón? (TL=Arriba Izq, MM=Centro, BR=Abajo Der).
  * **Salto Héctor:** ¿Hacia dónde se venció? (L=Izq, C=Centro, R=Der).
  * **Resultado:** Gol o No Gol.
* **Heatmap (Mapa de Calor):** Muestra visualmente las zonas donde más tiran los rivales de su categoría y el porcentaje de goles en cada zona.

---

## ⚙️ Zona de Administración

Accesible desde el icono de engranaje (arriba derecha).
* **Settings:** Actualizar foto de perfil y escudo del club.
* **Objetivos de Temporada:** Definir metas (ej: "10 Porterías a Cero").
* **Backup:** Descargar toda la base de datos en CSV (Excel).
* **Importador:** Cargar datos masivos antiguos.
* **Informe PDF:** Generar un reporte imprimible de la temporada.

---

## 🚀 ROADMAP: Guardian X (Next Gen)
*Conceptos estratégicos para convertir la herramienta en el arma definitiva.*

1.  **👁️ Vision Pro (Computer Vision):** Análisis de video automático. Ingesta de links de YouTube para generar *heatmaps* de posicionamiento y calcular tiempos de reacción (ms) sin etiquetado manual.
2.  **⌚ Bio-Telemetría Real (IoT):** Integración con wearables (Apple Watch/Whoop) para medir Carga Interna (FC media) y Sueño real (VFC), sustituyendo la percepción subjetiva.
3.  **🔮 El Oráculo (Predictive Analytics):** Proyección de carrera comparativa. Algoritmos que contrastan la curva de evolución de Héctor con datos históricos de porteros profesionales a su misma edad.
4.  **🧠 Diario de Voz (NLP):** Análisis de sentimiento post-partido. Grabación de audio donde la IA detecta frustración, ansiedad o euforia para gestionar la psicología deportiva.
5.  **♟️ Simulador Táctico (Gamificación):** Recreación digital 2D de los goles encajados para que el jugador tome la decisión correcta en el simulador ("Corregir el error virtualmente").
6.  **🛡️ Escudo Rival Dinámico (Scouting 2.0):** *Web Scraping* automático de actas digitales de la liga para detectar goleadores y patrones rivales sin entrada manual de datos.
7.  **🧬 Generador Visual (Video GenAI):** Sustitución de texto por video. La IA genera clips animados de avatares ejecutando la técnica exacta sugerida en el entrenamiento.
8.  **📉 'Moneyball' de Distribución:** Análisis de grafos de pases. Mapeo de conexiones para detectar con qué compañeros tiene mejor química y química negativa bajo presión.
9.  **🚦 Asistente de Banda (Live Mode):** Botonera simplificada para registro en tiempo real. Algoritmo de "Medio Tiempo" que envía una notificación al descanso con consejos tácticos basados en la 1ª parte.
10. **🕹️ Modo Legado (RPG Profundo):** Árbol de habilidades desbloqueable, equipamiento virtual legendario y crónicas de prensa generadas por IA tras cada victoria.

---

## 🏗️ Stack Tecnológico

* **Backend:** Scala (Cask, uPickle, Requests).
* **Database:** PostgreSQL (Neon Serverless).
* **Frontend:** ScalaTags (HTML server-side rendering) + Bootstrap 5 + Chart.js.
* **AI:** Google Gemini 1.5 Flash (vía API).

---

> *"No se trata de parar balones, se trata de evitar goles."*