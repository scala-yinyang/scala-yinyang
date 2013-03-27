#!/bin/sh
dir="./"

# Page rank
sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/PRankDelite.scala 20 21 "deg(t.Id)=PR" true ./prd'
sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/PRankDelite.scala 20 21 "g.notExisting(1)" true ./prdf'
#sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/PRank.scala 20 21 "deg(t.Id)=PR" true ./pr'
#sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/PRank.scala 20 21 "g2.notExisting(1)" true ./prf'

# SCC
sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/SCCDelite.scala 23 24 "G.AddEdge(2, 1)" true ./sccd'
sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/SCCDelite.scala 20 21 "G.notExisting(1)" true ./sccdf'
#sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/SCC.scala 23 24 "G.AddEdge(2, 1)" true ./scc'
#sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/SCC.scala 20 21 "G.notExisting(1)" true ./sccf'

# Conductance
sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/ConductanceDelite.scala 18 19 "G.AddEdge(s, G)" true ./condd'
sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/ConductanceDelite.scala 20 21 "G.notExisting(1)" true ./conddf'
#sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/Conductance.scala 18 19 "G.AddEdge(s, G)" true ./cond'
#sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/Conductance.scala 20 21 "G.notExisting(1)" true ./condf'


# 500
#sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/Synthetic500Delite.scala 250 251 "xxx" true ./500d'
#sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/Synthetic500Delite.scala 250 251 "g.notExisting(1)" true ./500df'
#sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/Synthetic500.scala 250 251 "xxx" true ./500'
#sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/Synthetic500.scala 250 251 "g.notExisting(1)" true ./500f'

# 1000
#sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/Synthetic1000Delite.scala 550 551 "xxx" true ./1000d'
#sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/Synthetic1000Delite.scala 550 551 "g.notExisting(1)" true ./1000df'
#sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/Synthetic1000.scala 550 551 "xxx" true ./1000'
#sbt 'delite-test/run /home/vjovanov/work/research/code/virt/mpde/components/delite-test/test/benchmarks/Synthetic1000.scala 550 551 "g.notExisting(1)" true ./1000f'