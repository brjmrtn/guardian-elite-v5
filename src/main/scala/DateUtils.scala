// ─────────────────────────────────────────────────────────────────────────────
// Restas de fechas centralizadas.
// NUNCA restar dos columnas DATE o TIMESTAMP directamente en PostgreSQL: la resta
// de timestamps da un 'interval' (no comparable con numeros) y mezclar tipos da
// "operator does not exist: timestamp - integer". Usar siempre estos helpers.
// (fecha - N con N entero sobre una columna DATE si es valido: da otra DATE.)
// ─────────────────────────────────────────────────────────────────────────────
object DateUtils {
  /** Dias (con decimales) entre dos expresiones SQL de fecha: col1 - col2.
   *  Cada operando va entre parentesis, asi admite LAG(...) OVER (...) o subconsultas. */
  def daysBetweenSQL(col1: String, col2: String): String =
    s"EXTRACT(EPOCH FROM (($col1)::timestamp - ($col2)::timestamp)) / 86400"

  /** Dias transcurridos desde la fecha `col` hasta hoy. */
  def daysFromTodaySQL(col: String): String =
    s"EXTRACT(EPOCH FROM (CURRENT_DATE::timestamp - ($col)::timestamp)) / 86400"

  /** Dias entre dos java.sql.Date (d2 - d1). */
  def daysBetween(d1: java.sql.Date, d2: java.sql.Date): Long =
    java.time.temporal.ChronoUnit.DAYS.between(d1.toLocalDate, d2.toLocalDate)
}
