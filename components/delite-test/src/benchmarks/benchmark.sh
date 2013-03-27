#!/bin/sh
dir="./bench-"

# Page rank
sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/PRankDelite.scala 20 21 "deg(t.Id)=PR" true ./bench-prd'
sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/PRankDelite.scala 20 21 "G.notExisting(1)" true ./bench-prdf'
sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/PRank.scala 32 33 "deg(t.Id)=PR" true ./bench-pr'
sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/PRank.scala 20 21 "G.notExisting(1)" true ./bench-prf'

# SCC
sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/SCCDelite.scala 23 24 "G.AddEdge(2, 1)" true ./bench-sccd'
sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/SCCDelite.scala 20 21 "G.notExisting(1)" true ./bench-sccdf'
sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/SCC.scala 23 24 "G.AddEdge(2, 1)" true ./bench-scc'
sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/SCC.scala 20 21 "G.notExisting(1)" true ./bench-sccf'

# Conductance
sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/ConductanceDelite.scala 18 19 "G.AddEdge(s, G)" true ./bench-condd'
sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/ConductanceDelite.scala 20 21 "G.notExisting(1)" true ./bench-conddf'
sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/Conductance.scala 18 19 "G.AddEdge(s, G)" true ./bench-cond'
sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/Conductance.scala 20 21 "G.notExisting(1)" true ./bench-condf'


# 500
#sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/Synthetic500Delite.scala 250 251 "xxx" true ./bench-500d'
#sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/Synthetic500Delite.scala 250 251 "g.notExisting(1)" true ./bench-500df'
#sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/Synthetic500.scala 250 251 "xxx" true ./bench-500'
#sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/Synthetic500.scala 250 251 "g.notExisting(1)" true ./bench-500f'

# 1000
#sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/Synthetic1000Delite.scala 550 551 "xxx" true ./bench-1000d'
#sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/Synthetic1000Delite.scala 550 551 "g.notExisting(1)" true ./bench-1000df'
#sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/Synthetic1000.scala 550 551 "xxx" true ./bench-1000'
#sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/Synthetic1000.scala 550 551 "g.notExisting(1)" true ./bench-1000f'