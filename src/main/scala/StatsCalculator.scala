import scala.math._

object StatsCalculator {

  // PESOS DE ATRIBUTOS PARA LA MEDIA DE PORTERO (GK)
  // Basado en pesos oficiales: DIV 20%, HAN 20%, KIC 15%, REF 20%, SPD 5%, POS 20%
  def calcularMediaGlobal(div: Double, han: Double, kic: Double, ref: Double, spd: Double, pos: Double): Int = {
    val raw = (div * 0.20) + (han * 0.20) + (kic * 0.15) + (ref * 0.20) + (spd * 0.05) + (pos * 0.20)
    Math.round(raw).toInt
  }

  // ALGORITMO DE CRECIMIENTO "TRINQUETE" (Ahora con Paradas)
  def calculateGrowth(
                       currentStats: PlayerCardData,
                       minutos: Int,
                       golesContra: Int,
                       valoracion: Double,
                       paradas: Int // <--- NUEVO ARGUMENTO QUE FALTABA
                     ): PlayerCardData = {

    // 1. FACTORES DE EXPERIENCIA (XP)
    // Jugar minutos da base. Hacerlo bien da multiplicador.
    val xpMinutos = Math.min(minutos * 0.002, 0.1) // Max 0.1 por jugar todo el partido
    val xpRendimiento = (valoracion / 10.0) * 0.3  // Max 0.3 por un 10

    // Bonus: Portería a cero (Solo si jugó más de 20 min)
    val bonusCleanSheet = if (golesContra == 0 && minutos > 20) 0.25 else 0.0

    // 🔥 FACTOR PARADAS: Cada 3 paradas da un plus extra (0.15)
    // Esto es clave: premia partidos con mucho trabajo aunque pierda
    val xpParadas = (paradas / 3.0) * 0.15

    // 2. APLICACIÓN POR ATRIBUTO (Lógica de Portero)

    // Estirada (DIV): Sube con rendimiento y algo con paradas
    val newDiv = currentStats.divRaw + xpMinutos + (xpRendimiento * 1.2) + (xpParadas * 0.5)

    // Manos (HAN): Sube mucho con paradas y portería a cero
    val newHan = currentStats.hanRaw + xpMinutos + xpRendimiento + bonusCleanSheet + xpParadas

    // Saque (KIC): Sube lento por minutos (se supone que saca)
    val newKic = currentStats.kicRaw + xpMinutos + 0.05

    // Reflejos (REF): EL QUE MÁS SUBE CON PARADAS
    val newRef = currentStats.refRaw + xpMinutos + (xpRendimiento * 1.5) + (xpParadas * 1.2)

    // Velocidad (SPD): Sube lento (físico)
    val newSpd = currentStats.spdRaw + 0.02

    // Posicionamiento (POS): Penaliza si encaja muchos goles (menos de 2 goles = sube)
    val factorGoles = if(golesContra < 2) 0.2 else 0.0
    val newPos = currentStats.posRaw + xpMinutos + factorGoles

    // 3. RECALCULAR MEDIA
    // Importante: Nunca bajamos stats, usamos Math.max para asegurar el "Trinquete"
    val finalDiv = Math.max(currentStats.divRaw, newDiv)
    val finalHan = Math.max(currentStats.hanRaw, newHan)
    val finalKic = Math.max(currentStats.kicRaw, newKic)
    val finalRef = Math.max(currentStats.refRaw, newRef)
    val finalSpd = Math.max(currentStats.spdRaw, newSpd)
    val finalPos = Math.max(currentStats.posRaw, newPos)

    val nuevaMedia = calcularMediaGlobal(finalDiv, finalHan, finalKic, finalRef, finalSpd, finalPos)

    // Devolvemos la nueva carta actualizada
    currentStats.copy(
      media = Math.max(currentStats.media, nuevaMedia), // La media tampoco baja
      divRaw = finalDiv, hanRaw = finalHan, kicRaw = finalKic,
      refRaw = finalRef, spdRaw = finalSpd, posRaw = finalPos,
      // Actualizamos los enteros para la visualización
      div = finalDiv.toInt, han = finalHan.toInt, kic = finalKic.toInt,
      ref = finalRef.toInt, spd = finalSpd.toInt, pos = finalPos.toInt
    )
  }
}