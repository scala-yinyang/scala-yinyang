// package collections.lms

// import scala.virtualization.lms.common._
// import scala.reflect.Manifest

// object LMSTPCH extends TpchBase with ScalaOpsPkgExp {
//   type OrdersRow = (Int, // _1: O_ORDERKEY
//   Int, // _2: O_CUSTKEY
//   Char, // _3: O_ORDERSTATUS
//   Double, // _4: O_TOTALPRICE
//   Date, // _5: O_ORDERDATE
//   String, // _6: O_ORDERPRIORITY
//   String, // _7: O_CLERK
//   Int, // _8: O_SHIPPRIORITY
//   String) // _9: O_COMMENT

//   /**
//    * A type alias for a tuple representing a row of the LINEITEM
//    * table.
//    */
//   type LineitemRow = (Int, // _1 : L_ORDERKEY
//   Int, // _2 : L_PARTKEY
//   Int, // _3 : L_SUPPKEY
//   Int, // _4 : L_LINENUMBER
//   Double, // _5 : L_QUANTITY
//   Double, // _6 : L_EXTENDEDPRICE
//   Double, // _7 : L_DISCOUNT
//   Double, // _8 : L_TAX
//   Char, // _9 : L_RETURNFLAG
//   Char, // _10: L_LINESTATUS
//   Date, // _11: L_SHIPDATE
//   Date, // _12: L_COMMITDATE
//   Date, // _13: L_RECEIPTDATE
//   String, // _14: L_SHIPINSTRUCT
//   String, // _15: L_SHIPMODE
//   String) // _16: L_COMMENT

//   /**
//    * TODO: Implement the "Shipping Modes and Order Priority Query"
//    * (Q12) from the TPC-H Benchmark:
//    *
//    * {{{
//    *   select
//    *     l_shipmode,
//    *     sum(case
//    *       when o_orderpriority = '1-URGENT'
//    *         or o_orderpriority = '2-HIGH'
//    *       then 1
//    *       else 0
//    *     end) as high_line_count,
//    *     sum(case
//    *       when o_orderpriority <> '1-URGENT'
//    *         and o_orderpriority <> '2-HIGH'
//    *       then 1
//    *       else 0
//    *     end) as low_line_count
//    *   from
//    *     orders,
//    *     lineitem
//    *   where
//    *     o_orderkey = l_orderkey
//    *     and l_shipmode    in ('[SHIPMODE1]', '[SHIPMODE2]')
//    *     and l_commitdate  <  l_receiptdate
//    *     and l_shipdate    <  l_commitdate
//    *     and l_receiptdate >= date '[DATE]'
//    *     and l_receiptdate <  date '[DATE]' + interval '1' year
//    *   group by
//    *     l_shipmode
//    *   order by
//    *     l_shipmode;
//    * }}}
//    *
//    * See http://www.tpc.org/tpch/spec/tpch2.15.0.pdf for more
//    * information on this particular TPC-H query.
//    */
//   def query12(
//     orders: Rep[List[OrdersRow]],
//     lineitem: Rep[List[LineitemRow]],
//     shipmode1: Rep[String],
//     shipmode2: Rep[String],
//     date: Rep[Date]): Rep[List[(String, Int, Int)]] = {

//     val m = orders flatMap {
//       o => lineitem map ((o, _)) // Cross join of `orders` with `lineitem`
//     } filter { p => // Filter on "where" conditions
//       (p._1._1 == p._2._1) && // o_orderkey = l_orderkey
//         ((p._2._15 == shipmode1) || // and l_shipmode in ('[SHIPMODE1]', '[SHIPMODE2]')
//           (p._2._15 == shipmode2)) &&
//           (p._2._12 < p._2._13) && // and l_commitdate  <  l_receiptdate
//           (p._2._11 < p._2._12) && // and l_shipdate    <  l_commitdate
//           (p._2._13 >= p._2._13) && // and l_receiptdate >= date '[DATE]'
//           (p._2._13 < p._2._13 + (1, 0, 0)) // and l_receiptdate <  date '[DATE]' + interval '1' year
//     } map { p => // Select `l_shipmode` and case expressions in sums
//       val b = ((p._1._6 == "1-URGENT") || (p._1._6 == "2-HIGH"))
//       (p._2._15, if (b) 1 else 0, if (!b) 1 else 0)
//     } groupBy ((_._1)) // Group by `l_shipmode`

//     // Iterate over the key of the grouped rows and compute the sums
//     (m.keys.toList map { k: Rep[String] =>
//       m(k).foldLeft((k, unit(0), unit(0))) { (acc, row) =>
//         (acc._1, acc._2 + row._2, acc._3 + row._3)
//       }
//     }) sortBy ((_._1)) // Sort by `l_shipmode`
//   }

//   /**
//    * TODO: Implement the "Shipping Modes and Order Priority Query"
//    * (Q1) from the TPC-H Benchmark:
//    *
//    * {{{
//    *   select
//    *     l_returnflag,
//    *     l_linestatus,
//    *     sum(l_quantity) as sum_qty,
//    *     sum(l_extendedprice) as sum_base_price,
//    *     sum(l_extendedprice*(1-l_discount)) as sum_disc_price,
//    *     sum(l_extendedprice*(1-l_discount)*(1+l_tax)) as sum_charge,
//    *     avg(l_quantity) as avg_qty,
//    *     avg(l_extendedprice) as avg_price,
//    *     avg(l_discount) as avg_disc,
//    *     count(*) as count_order
//    *   from
//    *     lineitem
//    *   where
//    *     l_shipdate <= date '1998-12-01' - interval '[DELTA]' day (3)
//    *   group by
//    *     l_returnflag,
//    *     l_linestatus
//    *   order by
//    *     l_returnflag,
//    *     l_linestatus;
//    * }}}
//    *
//    * This query was modified to be able to use only supportted tuples. Here is the modified version:
//    *
//    * {{{
//    *   select
//    *     l_returnflag,
//    *     l_linestatus,
//    *     sum(l_quantity) as sum_qty,
//    *     sum(l_extendedprice) as sum_base_price,
//    *     sum(l_extendedprice*(1-l_discount)) as sum_disc_price,
//    *     sum(l_extendedprice*(1-l_discount)*(1+l_tax)) as sum_charge,
//    *     avg(l_quantity) as avg_qty,
//    *     avg(l_extendedprice) as avg_price,
//    *     avg(l_discount) as avg_disc,
//    *   from
//    *     lineitem
//    *   where
//    *     l_shipdate <= date '1998-12-01' - interval '[DELTA]' day (3)
//    *   group by
//    *     l_returnflag,
//    *     l_linestatus
//    *   order by
//    *     l_returnflag,
//    *     l_linestatus;
//    * }}}
//    *
//    * See http://www.tpc.org/tpch/spec/tpch2.15.0.pdf for more
//    * information on this particular TPC-H query.
//    */
//   def query1(
//     lineitem: Rep[List[LineitemRow]],
//     Delta: Rep[Int]): Rep[List[(Char, Char, Double, Double, Double, Double, Double, Double, Double)]] = {
//     def grouping(
//       e: Rep[(Int, Int, Int, Int, Double, Double, Double, Double, Char, Char, Date, Date, Date, String, String, String)]): Rep[(Char, Char, Double, Double, Double, Double, Int, Int, Int)] =
//       (e._9, e._10, e._5, e._6, e._7, e._8, unit(0), unit(0), unit(0))
//     var m = (lineitem filter { e => e._11 == (Date("1998-12-01")) - (0, 0, Delta * 3) }).map(x => grouping(x)).groupBy { (e) => (e._1, e._2) }
//     m.keys.toList map {
//       k =>
//         (k, m(k).length, m(k).foldLeft((unit(0.0), unit(0.0), unit(0.0), unit(0.0), unit(0.0)))((acc, te) =>
//           (acc._1 + te._3, acc._2 + te._4, acc._3 + te._4 * (unit(1.0) - te._5), acc._4 + te._6, acc._5 + te._4 * (unit(1.0) - te._4) * (unit(1.0) + te._6))))
//     } map { p => (p._1._1, p._1._2, p._3._1, p._3._2, p._3._3, p._3._5, p._3._1 / p._2, p._3._2 / p._2, p._3._4 / p._2)
//     } sortBy (e => (e._1, e._2))
//   }

//   /**
//    * TODO: Implement the "Shipping Modes and Order Priority Query"
//    * (Q4) from the TPC-H Benchmark:
//    *
//    * {{{
//    *   select
//    *     o_orderpriority,
//    *     count(*) as order_count
//    *   from
//    *     orders
//    *   where
//    *     o_orderdate >= date '[DATE]'
//    *       and o_orderdate < date '[DATE]' + interval '3' month
//    *       and exists (
//    *             select
//    *               *
//    *             from
//    *               lineitem
//    *             where
//    *               l_orderkey = o_orderkey
//    *                 and l_commitdate < l_receiptdate
//    *       )
//    *     group by
//    *       o_orderpriority
//    *     order by
//    *       o_orderpriority;
//    * }}}
//    *
//    * See http://www.tpc.org/tpch/spec/tpch2.15.0.pdf for more
//    * information on this particular TPC-H query.
//    */
//   def query4(
//     orders: Rep[List[OrdersRow]],
//     lineitem: Rep[List[LineitemRow]],
//     shipmode1: Rep[String],
//     shipmode2: Rep[String],
//     date: Rep[Date]): Rep[List[(String, Int)]] = {

//     var m = orders filter {
//       o =>
//         o._5 >= date && o._5 < date + (0, 3, 0) &&
//           !((lineitem filter (l => l._1 == o._1 && l._12 < l._13)).isEmpty)
//     } map (o => o._6) groupBy (e => e)
//     m.keys.toList map (k => (k, (m(k).length))) sortBy ((_._1))
//   }

//   /**
//    * TODO: Implement the "Shipping Modes and Order Priority Query"
//    * (Q6) from the TPC-H Benchmark:
//    *
//    * {{{
//    *   select
//    *     sum(l_extendedprice*l_discount) as revenue
//    *   from
//    *     lineitem
//    *   where
//    *     l_shipdate >= date '[DATE]'
//    *      and l_shipdate < date '[DATE]' + interval '1' year
//    *      and l_discount between [DISCOUNT] - 0.01 and [DISCOUNT] + 0.01
//    *      and l_quantity < [QUANTITY];
//    * }}}
//    *
//    * See http://www.tpc.org/tpch/spec/tpch2.15.0.pdf for more
//    * information on this particular TPC-H query.
//    */
//   def query6(
//     orders: Rep[List[OrdersRow]],
//     lineitem: Rep[List[LineitemRow]],
//     shipmode1: Rep[String],
//     shipmode2: Rep[String],
//     date: Rep[Date],
//     discount: Rep[Double],
//     quantity: Rep[Int]): Rep[Double] = {

//     (lineitem filter { l =>
//       (l._11 >= date) &&
//         (l._11 < date + (1, 0, 0)) //&&
//       (discount - 0.01 > l._7) &&
//         (l._7 > discount + 0.01) &&
//         (l._5 < quantity)
//     }).foldLeft(unit(0.0))(
//       (acc: Rep[Double], te: Rep[LineitemRow]) => acc + te._6 * te._7)
//   }

//   def apply(
//     orders: Rep[List[OrdersRow]],
//     lineitem: Rep[List[LineitemRow]],
//     shipmode1: Rep[String],
//     shipmode2: Rep[String],
//     date: Rep[Date]): Rep[List[(String, Int, Int)]] = query12(orders, lineitem, shipmode1, shipmode2, date)
// }