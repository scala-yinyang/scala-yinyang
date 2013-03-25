package collections.yinyang

object YYTPCH {
  import collections._
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

  /**
   * TODO: Implement the "Shipping Modes and Order Priority Query"
   * (Q12) from the TPC-H Benchmark:
   *
   * {{{
   *   select
   *     l_shipmode,
   *     sum(case
   *       when o_orderpriority = '1-URGENT'
   *         or o_orderpriority = '2-HIGH'
   *       then 1
   *       else 0
   *     end) as high_line_count,
   *     sum(case
   *       when o_orderpriority <> '1-URGENT'
   *         and o_orderpriority <> '2-HIGH'
   *       then 1
   *       else 0
   *     end) as low_line_count
   *   from
   *     orders,
   *     lineitem
   *   where
   *     o_orderkey = l_orderkey
   *     and l_shipmode    in ('[SHIPMODE1]', '[SHIPMODE2]')
   *     and l_commitdate  <  l_receiptdate
   *     and l_shipdate    <  l_commitdate
   *     and l_receiptdate >= date '[DATE]'
   *     and l_receiptdate <  date '[DATE]' + interval '1' year
   *   group by
   *     l_shipmode
   *   order by
   *     l_shipmode;
   * }}}
   *
   * See http://www.tpc.org/tpch/spec/tpch2.15.0.pdf for more
   * information on this particular TPC-H query.
   */
  def query12(
    orders: List[OrdersRow],
    lineitem: List[LineitemRow],
    shipmode1: String,
    shipmode2: String,
    date: Date): List[(String, Int, Int)] = {

    val m = orders flatMap {
      o ⇒ lineitem map ((o, _)) // Cross join of `orders` with `lineitem`
    } filter { p ⇒ // Filter on "where" conditions
      (p._1._1 == p._2._1) && // o_orderkey = l_orderkey
        ((p._2._15 == shipmode1) || // and l_shipmode in ('[SHIPMODE1]', '[SHIPMODE2]')
          (p._2._15 == shipmode2)) &&
          (p._2._12 < p._2._13) && // and l_commitdate  <  l_receiptdate
          (p._2._11 < p._2._12) && // and l_shipdate    <  l_commitdate
          (p._2._13 >= p._2._13) && // and l_receiptdate >= date '[DATE]'
          (p._2._13 < p._2._13 + (1, 0, 0)) // and l_receiptdate <  date '[DATE]' + interval '1' year
    } map { p ⇒ // Select `l_shipmode` and case expressions in sums
      val b = ((p._1._6 == "1-URGENT") || (p._1._6 == "2-HIGH"))
      (p._2._15, if (b) 1 else 0, if (!b) 1 else 0)
    } groupBy ((_._1)) // Group by `l_shipmode`

    // Iterate over the key of the grouped rows and compute the sums
    (m.keys.toList map { k: String ⇒
      m(k).foldLeft((k, 0, 0)) { (acc, row) ⇒
        (acc._1, acc._2 + row._2, acc._3 + row._3)
      }
    }) sortBy ((_._1)) // Sort by `l_shipmode`
  }
}