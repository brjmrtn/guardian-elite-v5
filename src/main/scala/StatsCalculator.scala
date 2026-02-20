import scala.math._

object StatsCalculator {

  // PESOS OFICIALES (FUT STYLE)
  def calcularMediaGlobal(div: Double, han: Double, kic: Double, ref: Double, spd: Double, pos: Double): Int = {
    val raw = (div * 0.20) + (han * 0.20) + (kic * 0.15) + (ref * 0.20) + (spd * 0.05) + (pos * 0.20)
    Math.round(raw).toInt
  }

  // ALGORITMO "TRINQUETE" MEJORADO CON JUEGO DE PIES
  def calculateGrowth(
                       currentStats: PlayerCardData,
                       minutos: Int,
                       golesContra: Int,
                       valoracion: Double,
                       paradas: Int,
                       pcTot: Int, pcOk: Int, plTot: Int, plOk: Int // <--- NUEVOS DATOS DE PASES
                     ): PlayerCardData = {

    // 1. XP BASE
    // Umbral de rendimiento: el trinquete "frena" si la valoracion no llega a 5.0.
    // El jugador no baja (esa es la garantia del trinquete), pero tampoco sube
    // simplemente por haber jugado — necesita un minimo de calidad en el partido.
    val notaSuficiente = valoracion >= 5.0
    val xpMinutos = if (notaSuficiente) Math.min(minutos * 0.002, 0.1) else 0.0
    val xpRendimiento = (valoracion / 10.0) * 0.15   // Puede ser pequeño pero nunca negativo

    // 2. CÁLCULO DE XP POR DISTRIBUCIÓN (EL "MODULO EDERSON")
    var xpKic = 0.0

    // Pases Cortos: Premia consistencia.
    if (pcTot > 0) {
      val pctCorto = pcOk.toDouble / pcTot.toDouble
      // Si acierta > 80% sube, si no, se mantiene o sube muy poco
      xpKic += (pctCorto * 0.1) + (pcOk * 0.01)
    }

    // Pases Largos: Premia riesgo y visión. Valen el doble.
    if (plTot > 0) {
      val pctLargo = plOk.toDouble / plTot.toDouble
      // El pase largo es difícil. Un 50% de acierto ya es bueno.
      xpKic += (pctLargo * 0.2) + (plOk * 0.02)
    }

    // Bonus por portería a cero en KIC (confianza)
    if (golesContra == 0 && minutos > 20) xpKic += 0.1

    // 3. CÁLCULO DE PARADAS (REFLEJOS/MANOS)
    val xpParadas = Math.min(paradas * 0.05, 0.5)

    // 4. ACTUALIZACIÓN DE STATS
    val newDiv = currentStats.divRaw + xpMinutos + xpRendimiento + (xpParadas * 0.5)
    val newHan = currentStats.hanRaw + xpMinutos + (if(golesContra==0) 0.1 else 0) + (xpParadas * 0.8)

    // KIC: Ahora depende puramente de lo que haga con los pies + minutos
    val newKic = currentStats.kicRaw + xpMinutos + xpKic

    val newRef = currentStats.refRaw + xpMinutos + (xpRendimiento * 1.5) + (xpParadas * 1.2)
    val newSpd = currentStats.spdRaw + 0.02
    val factorGoles = if(golesContra < 2) 0.2 else 0.0
    val newPos = currentStats.posRaw + xpMinutos + factorGoles

    // 5. TRINQUETE (NUNCA BAJA)
    val finalDiv = Math.max(currentStats.divRaw, newDiv)
    val finalHan = Math.max(currentStats.hanRaw, newHan)
    val finalKic = Math.max(currentStats.kicRaw, newKic) // Solo sube si pasa bien
    val finalRef = Math.max(currentStats.refRaw, newRef)
    val finalSpd = Math.max(currentStats.spdRaw, newSpd)
    val finalPos = Math.max(currentStats.posRaw, newPos)

    val nuevaMedia = calcularMediaGlobal(finalDiv, finalHan, finalKic, finalRef, finalSpd, finalPos)

    currentStats.copy(
      media = nuevaMedia,
      div = finalDiv.toInt, han = finalHan.toInt, kic = finalKic.toInt,
      ref = finalRef.toInt, spd = finalSpd.toInt, pos = finalPos.toInt,
      divRaw = finalDiv, hanRaw = finalHan, kicRaw = finalKic,
      refRaw = finalRef, spdRaw = finalSpd, posRaw = finalPos
    )
  }
  def calculateACWR(acuteLoads: Seq[Double], chronicLoads: Seq[Double]): Double = {
    val acuteAvg = if (acuteLoads.nonEmpty) acuteLoads.sum / 7.0 else 0.0
    val chronicAvg = if (chronicLoads.nonEmpty) chronicLoads.sum / 28.0 else 1.0

    if (chronicAvg > 0) acuteAvg / chronicAvg else 0.0
  }

  // --- BONUS JUDO: Mejora Agilidad y Valentía ---
  def applyJudoBonus(currentStats: PlayerCardData): PlayerCardData = {
    val bonusRef = 0.08 // El judo mejora los reflejos por las caídas
    val bonusPos = 0.05 // Mejora el posicionamiento y la valentía

    val finalRef = currentStats.refRaw + bonusRef
    val finalPos = currentStats.posRaw + bonusPos

    val nuevaMedia = calcularMediaGlobal(
      currentStats.divRaw, currentStats.hanRaw, currentStats.kicRaw,
      finalRef, currentStats.spdRaw, finalPos
    )

    currentStats.copy(
      media = nuevaMedia,
      ref = finalRef.toInt,
      pos = finalPos.toInt,
      refRaw = finalRef,
      posRaw = finalPos
    )
  }
}