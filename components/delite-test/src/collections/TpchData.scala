package collections

object YYTpchData extends TpchDataLike {
  type Date = collections.Date
  def date(str: String): Date = Date(str)
}

trait TpchDataLike {
  type Date

  def date(str: String): Date

  /*
   * A type alias for a tuple representing a row of the ORDERS table.
   */
  type OrdersRow = (Int, // _1: O_ORDERKEY
  Int, // _2: O_CUSTKEY
  Char, // _3: O_ORDERSTATUS
  Double, // _4: O_TOTALPRICE
  Date, // _5: O_ORDERDATE
  String, // _6: O_ORDERPRIORITY
  String, // _7: O_CLERK
  Int, // _8: O_SHIPPRIORITY
  String) // _9: O_COMMENT

  /**
   * A type alias for a tuple representing a row of the LINEITEM
   * table.
   */
  type LineitemRow = (Int, // _1 : L_ORDERKEY
  Int, // _2 : L_PARTKEY
  Int, // _3 : L_SUPPKEY
  Int, // _4 : L_LINENUMBER
  Double, // _5 : L_QUANTITY
  Double, // _6 : L_EXTENDEDPRICE
  Double, // _7 : L_DISCOUNT
  Double, // _8 : L_TAX
  Char, // _9 : L_RETURNFLAG
  Char, // _10: L_LINESTATUS
  Date, // _11: L_SHIPDATE
  Date, // _12: L_COMMITDATE
  Date, // _13: L_RECEIPTDATE
  String, // _14: L_SHIPINSTRUCT
  String, // _15: L_SHIPMODE
  String) // _16: L_COMMENT

  /** Test entries from the ORDERS table. */
  val orders: List[OrdersRow] = List(
    (533997, 5805899, 'F', 245384.60, date("1992-07-27"), "2-HIGH", "Clerk#000030791", 0, "counts cajole express package"),
    (533998, 4731317, 'F', 216295.79, date("1993-11-24"), "4-NOT SPECIFIED", "Clerk#000000889", 0, "g furiously carefully express accounts. even packa"),
    (533999, 4232405, 'O', 37382.44, date("1997-04-12"), "2-HIGH", "Clerk#000039728", 0, "xes. slyly regular accounts cajo"),
    (534024, 4018079, 'O', 147759.78, date("1998-03-04"), "5-LOW", "Clerk#000097097", 0, " final pinto beans. finally final instructions haggle fluffily "),
    (534025, 1839745, 'O', 149253.25, date("1996-06-08"), "4-NOT SPECIFIED", "Clerk#000003799", 0, "ions detect about th"),
    (534026, 5593220, 'F', 121492.46, date("1993-12-07"), "3-MEDIUM", "Clerk#000039824", 0, "ages. quickly express theodolites alongside of the packages lose about the fi"),
    (534027, 246475, 'P', 140190.15, date("1995-04-17"), "1-URGENT", "Clerk#000008258", 0, "across the quickly unusual deposits use furiously even packages-- "),
    (534028, 2476157, 'O', 259982.48, date("1998-02-25"), "2-HIGH", "Clerk#000080633", 0, "c instructions haggle a"),
    (534029, 6769840, 'F', 74154.01, date("1992-10-28"), "1-URGENT", "Clerk#000093827", 0, "y unusual instructions hang blithely at the pend"),
    (534030, 5681488, 'O', 5355.49, date("1998-02-17"), "4-NOT SPECIFIED", "Clerk#000046231", 0, " regular accounts integrate blithely. unusual deposits "))

  /** Test entries from the LINEITEMS table. */
  val lineitem: List[LineitemRow] = List(
    (533997, 9168238, 418248, 1, 39, 50925.42, 0.07, 0.08, 'N', 'O', date("1996-04-08"), date("1996-04-14"), date("1996-04-16"), "TAKE BACK RETURN", "TRUCK", "theodolites wake blithely after "),
    (533997, 10572491, 322522, 2, 40, 62518.80, 0.07, 0.06, 'N', 'O', date("1996-05-09"), date("1996-06-12"), date("1996-06-06"), "DELIVER IN PERSON", "MAIL", "its integrate slyly. bl"),
    (533998, 6457304, 207323, 1, 36, 45395.28, 0.08, 0.00, 'R', 'F', date("1992-09-27"), date("1992-07-30"), date("1992-10-25"), "TAKE BACK RETURN", "TRUCK", "carefully according "),
    (533999, 3107319, 607326, 1, 15, 19892.40, 0.00, 0.01, 'N', 'O', date("1996-04-29"), date("1996-06-25"), date("1996-05-07"), "COLLECT COD", "MAIL", "ades. regular dep"),
    (533999, 4695419, 445432, 2, 4, 5656.72, 0.08, 0.08, 'N', 'O', date("1996-05-29"), date("1996-06-15"), date("1996-06-18"), "DELIVER IN PERSON", "FOB", "into beans integrate slyly instead of t"),
    (533999, 15890555, 640601, 3, 37, 57156.12, 0.10, 0.02, 'N', 'O', date("1996-05-03"), date("1996-07-09"), date("1996-05-16"), "NONE", "TRUCK", "ent requests haggle carefully again"),
    (533999, 12546113, 796126, 4, 19, 22011.31, 0.05, 0.01, 'N', 'O', date("1996-06-02"), date("1996-07-03"), date("1996-06-04"), "COLLECT COD", "FOB", "ainst the care"),
    (533999, 2506940, 506941, 5, 30, 58404.60, 0.06, 0.07, 'N', 'O', date("1996-05-07"), date("1996-06-13"), date("1996-06-01"), "TAKE BACK RETURN", "MAIL", "y ironic asymptotes. ironic, even foxes a"),
    (534024, 16738857, 488906, 1, 6, 11370.12, 0.10, 0.03, 'N', 'O', date("1996-01-25"), date("1995-11-27"), date("1996-02-11"), "COLLECT COD", "AIR", "unts. regular foxes hang furiousl"),
    (534025, 8697376, 947385, 1, 16, 21967.04, 0.05, 0.06, 'N', 'O', date("1996-07-31"), date("1996-06-23"), date("1996-08-28"), "COLLECT COD", "REG AIR", "beans must x-ray blithel"),
    (534025, 16790044, 790045, 2, 16, 18131.36, 0.07, 0.02, 'N', 'O', date("1996-05-15"), date("1996-06-29"), date("1996-05-25"), "TAKE BACK RETURN", "SHIP", "lites play quickly furiously ironic dep"),
    (534025, 10263072, 263073, 3, 19, 19656.64, 0.04, 0.06, 'N', 'O', date("1996-05-19"), date("1996-07-28"), date("1996-06-14"), "COLLECT COD", "TRUCK", "ep. pending requests thrash carefull"),
    (534025, 11442903, 692915, 4, 11, 20298.63, 0.01, 0.03, 'N', 'O', date("1996-06-22"), date("1996-06-10"), date("1996-07-02"), "TAKE BACK RETURN", "TRUCK", "ges mold carefully. packages wake fur"),
    (534025, 855486, 105487, 5, 8, 11531.52, 0.02, 0.06, 'N', 'O', date("1996-07-27"), date("1996-07-20"), date("1996-08-10"), "TAKE BACK RETURN", "RAIL", " regular foxes sleep blithely a"),
    (534026, 6101642, 351649, 1, 12, 19720.08, 0.01, 0.03, 'A', 'F', date("1992-10-26"), date("1992-11-03"), date("1992-11-21"), "NONE", "TRUCK", "nal pinto beans. blithely i"),
    (534026, 10281952, 781973, 2, 43, 83137.92, 0.01, 0.08, 'A', 'F', date("1992-12-09"), date("1992-10-22"), date("1992-12-20"), "DELIVER IN PERSON", "AIR", "lly according to the fluffily "),
    (534026, 8753183, 753184, 3, 47, 58080.25, 0.08, 0.00, 'A', 'F', date("1992-11-03"), date("1992-12-02"), date("1992-11-12"), "NONE", "REG AIR", "s. quickly express deposits may sleep caref"),
    (534026, 14738137, 738138, 4, 3, 3523.20, 0.04, 0.02, 'A', 'F', date("1992-09-22"), date("1992-11-17"), date("1992-09-25"), "TAKE BACK RETURN", "TRUCK", "c packages nag fu"),
    (534027, 14603049, 103078, 1, 49, 46614.19, 0.01, 0.03, 'A', 'F', date("1992-04-23"), date("1992-03-21"), date("1992-05-07"), "COLLECT COD", "MAIL", "uses. ironic deposits"),
    (534028, 16383173, 633190, 1, 19, 23851.84, 0.03, 0.00, 'R', 'F', date("1994-04-15"), date("1994-03-11"), date("1994-04-21"), "DELIVER IN PERSON", "RAIL", "riously al"))

}