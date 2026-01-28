# 🛡️ GUARDIAN ELITE v4.1 | High Performance Center

> **"El talento te lleva al área, el dato te hace dueño de la portería."**

**Guardian Elite** no es una simple app de estadísticas. Es un ecosistema integral de gestión deportiva diseñado para monitorizar, analizar y potenciar la carrera de un portero joven en desarrollo (**Héctor**). Combina la analítica de datos profesional (Big Data), la gamificación (estilo videojuego) y la Inteligencia Artificial Generativa (Gemini AI) para maximizar el desarrollo técnico, táctico, físico y mental.

![Version](https://img.shields.io/badge/Version-4.1_Smart_Data-blue?style=for-the-badge&logo=scala) ![Database](https://img.shields.io/badge/Database-PostgreSQL_Neon-green?style=for-the-badge&logo=postgresql) ![AI](https://img.shields.io/badge/AI-Gemini_Powered-orange?style=for-the-badge&logo=google-gemini)

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

## 📖 MANUAL DE USO DETALLADO

### 1. 🏠 DASHBOARD (Centro de Mando)
*La primera pantalla que ves. Aquí se centraliza la inteligencia.*

#### 👤 La Carta FUT (Player Card)
Visualización gamificada del nivel actual del portero.
* **Algoritmo de Trinquete:** La media (0-99) está calculada mediante un algoritmo matemático personalizado. La clave es que **la media nunca baja**. Si el portero tiene un mal partido, la media se estanca, pero no retrocede. Esto protege la confianza y moral del niño.
* **Barra de XP:** Muestra el progreso decimal exacto para subir al siguiente punto de media.
* **Stats Dinámicas:** 6 atributos (DIV, HAN, KIC, REF, SPD, POS) que evolucionan según lo que pase en el campo.

#### 🕵️ Inteligencia de Datos (Smart Insights) [NUEVO v4.1]
Un sistema pasivo que cruza datos en segundo plano para detectar anomalías:
* **⚠️ Alerta "Torpeza del Estirón":** El sistema cruza la velocidad de crecimiento. Si detecta un estirón (>0.5cm/mes) y simultáneamente una bajada en la nota de coordinación técnica, lanza una alerta para avisar al entrenador de que el fallo es biomecánico, no técnico.
* **🔍 Detector de Patrones de Dolor:** Analiza si los registros de dolor físico coinciden recurrentemente con un tipo de superficie (césped duro) o un tipo de entrenamiento específico.

#### 🧠 IA Neuro-Scout
Un asistente basado en **Gemini AI** que analiza 4 factores antes de cada sesión:
1.  **Bio-Ritmo:** ¿Durmió bien? ¿Tiene energía?
2.  **Racha:** ¿Viene de ganar o perder?
3.  **Agenda:** ¿Quién es el próximo rival?
4.  **Contexto:** ¿Está en fase de crecimiento?
* **Resultado:** Ofrece un consejo táctico o motivacional ultra-corto y específico para el día de hoy.

---

### 2. ⚽ MATCH CENTER (El Partido)
*Herramienta de registro profesional post-partido.*

#### 🎯 El "Módulo Ederson" (Distribución)
Control obsesivo del juego de pies, vital para el portero moderno. Afecta directamente al atributo **KIC** (Kicking).
* **Pase Corto:** Registro de acierto/fallo en pases de seguridad.
* **Pase Largo:** Registro de acierto/fallo en desplazamientos que rompen líneas de presión.

#### 🥅 Rejilla Portería (3x3)
Un sistema de coordenadas para analizar la eficacia bajo palos.
* **Modo Parada:** Registra en qué zona (Escuadra, Raso, Centro...) se detuvo el balón.
* **Modo Gol:** Registra por dónde entró el balón para detectar debilidades (ej: "Le marcan mucho por abajo a la izquierda").

#### 🗺️ Heatmap de Campo (Juego Real) [NUEVO v4.1]
Registro posicional de las intervenciones fuera de la portería.
* **Funcionamiento:** Un mapa interactivo del campo verde. Permite marcar dónde tocó el balón Héctor (cortes de líbero, inicios de jugada fuera del área).
* **Objetivo:** Verificar si juega adelantado y valiente o si se queda pegado a la línea de gol.

#### 🎙️ Diario de Voz (Psicólogo IA) [NUEVO v4.1]
Herramienta de descarga emocional.
* **Grabar:** Permite al niño grabar un audio de 30s contando cómo se sintió en el partido.
* **Análisis:** La IA procesa el audio, transcribe el texto y detecta el **estado emocional real** (Frustración, Euforia, Calma) para dar consejos al padre sobre cómo gestionar la charla post-partido.

---

### 3. 🧬 BIO & ENTRENO (El Laboratorio)
*Gestión fisiológica y diseño de sesiones.*

#### 🩺 Wellness & Crecimiento
* **Diario:** Registro matutino de Calidad de Sueño, Nivel de Energía, Dolor y Estado de Ánimo.
* **Growth Tracker:** Registro de Altura y Peso. Calcula automáticamente la velocidad de crecimiento (cm/mes) para predecir riesgos de lesión o descoordinación.

#### 🏋️ Generador de Sesiones "Híbrido" [NUEVO v4.1]
Un diseñador de entrenamientos inteligente con memoria.
* **Contexto:** Al pedir una sesión (ej: "Papá Portero"), el sistema consulta la base de datos para ver qué hizo ayer.
    * *Si ayer tuvo partido duro:* Genera sesión de Recuperación/Técnica suave.
    * *Si ayer descansó:* Genera sesión de Alta Intensidad/Carga.
    * *Si viene de Academia:* Prioriza el Juego Real sobre la técnica analítica.

---

### 4. 🕵️ SCOUTING (Black Book)
Base de datos de inteligencia competitiva.
* **Buscador:** Encuentra historial contra cualquier equipo.
* **Ficha Rival:** Estilo de juego (Directo/Combinativo), Jugadores Clave y Notas del entrenador.
* **Historial:** Muestra automáticamente resultados anteriores contra ese equipo para preparar el partido.

---

### 5. 🧤 GARAGE (Material)
Gestión profesional del equipamiento.
* **Vida Útil:** Define cuántos usos tienen unos guantes o botas.
* **Semáforo:** La barra de vida baja con cada partido/entreno.
    * 🟢 Verde: Óptimo.
    * 🟡 Amarillo: Desgaste.
    * 🔴 Rojo: Crítico/Cambiar (Alerta visual).

---

### 6. 📋 PIZARRA & 7. 🥅 PENALTY LAB
* **Pizarra Táctica:** Canvas digital multitáctil para dibujar explicaciones rápidas en el descanso.
* **Penalty Lab:** Base de datos de penaltis. Registra zona de tiro y zona de salto. Genera un mapa de calor de por dónde suelen tirar los rivales de su categoría.

---

## 🚀 ROADMAP: Próximos Pasos (v4.2+)

El plan de desarrollo aprobado para convertir la app en una herramienta de **Alto Rendimiento**:

### 📂 BLOQUE A: VIDEOANÁLISIS Y BIOMECÁNICA
1.  **📹 La "Videoteca":** Playlist automática de mejores paradas para refuerzo positivo pre-partido.
2.  **🩺 Auditor Técnico:** Checklist para detectar vicios técnicos en video (paso negativo, manos planas).
3.  **📐 Simulador "Bisectriz":** Herramienta visual para enseñar posicionamiento y reducción de ángulos.

### 📂 BLOQUE B: SPORTS SCIENCE (Prevención)
4.  **📉 Ratio ACWR:** Cálculo de carga Aguda/Crónica para prevenir lesiones por sobreentrenamiento.
5.  **🩹 Mapa 3D Lesiones:** Registro visual ("Body Paint") de zonas de dolor recurrentes.
6.  **🌧️ Factor Clima:** Análisis de cómo la lluvia/viento afecta al rendimiento (blocajes).

### 📂 BLOQUE C: INTELIGENCIA TÁCTICA
7.  **⏱️ Momentum:** Gráfica lineal del partido para detectar desconexiones mentales.
8.  **🕸️ Telaraña de Distribución:** Gráfico de red para evitar pases predecibles.
9.  **♟️ Pizarra ABP:** Editor de estrategias defensivas (córners/faltas).

---

## 🏗️ Stack Tecnológico

* **Backend:** Scala (Framework Cask, uPickle, Requests).
* **Database:** PostgreSQL (Alojada en Neon Serverless).
* **Frontend:** HTML renderizado en servidor (ScalaTags) + Bootstrap 5.
* **Gráficos:** Chart.js para radares y curvas evolutivas.
* **AI:** Google Gemini 1.5 Flash (vía API).
---

## 🏗️ Stack Tecnológico

* **Backend:** Scala (Cask, uPickle, Requests).
* **Database:** PostgreSQL (Neon Serverless).
* **Frontend:** ScalaTags (HTML server-side rendering) + Bootstrap 5 + Chart.js.
* **AI:** Google Gemini 1.5 Flash (vía API).

---

> *"No se trata de parar balones, se trata de evitar goles."*