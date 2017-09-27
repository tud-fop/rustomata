
mkdir res/

while read -r n; do
  make bench -B NAME=corp/$n PTK=5 WORDS=3 NFA=true EQC=corp/$n
  mkdir res/$n-05
  cp benchmark-results.txt res/$n-05/results.txt
  cp benchmark.log res/$n-05/results.log

  make bench -B NAME=corp/$n PTK=5 WORDS=3 NFA=true EQC=corp/${n}_2
  mkdir res/$n-05-2
  cp benchmark-results.txt res/$n-05-2/results.txt
  cp benchmark.log res/$n-05-2/results.log

  make bench -B NAME=corp/$n PTK=10 WORDS=3 NFA=true EQC=corp/$n
  mkdir res/$n-10
  cp benchmark-results.txt res/$n-10/results.txt
  cp benchmark.log res/$n-10/results.log

  make bench -B NAME=corp/$n PTK=10 WORDS=3 NFA=true EQC=corp/${n}_2
  mkdir res/$n-10-2
  cp benchmark-results.txt res/$n-10-2/results.txt
  cp benchmark.log res/$n-10-2/results.log

  make bench -B NAME=corp/$n PTK=15 WORDS=3 NFA=true EQC=corp/$n
  mkdir res/$n-15
  cp benchmark-results.txt res/$n-15/results.txt
  cp benchmark.log res/$n-15/results.log

  make bench -B NAME=corp/$n PTK=15 WORDS=3 NFA=true EQC=corp/${n}_2
  mkdir res/$n-15-2
  cp benchmark-results.txt res/$n-15-2/results.txt
  cp benchmark.log res/$n-15-2/results.log

  make bench -B NAME=corp/$n PTK=20 WORDS=3 NFA=true EQC=corp/$n
  mkdir res/$n-20
  cp benchmark-results.txt res/$n-20/results.txt
  cp benchmark.log res/$n-20/results.log

  make bench -B NAME=corp/$n PTK=20 WORDS=3 NFA=true EQC=corp/${n}_2
  mkdir res/$n-20-2
  cp benchmark-results.txt res/$n-20-2/results.txt
  cp benchmark.log res/$n-20-2/results.log
done < corp/bench_no_nfa.list

while read -r n; do
  make bench -B NAME=corp/$n PTK=10 WORDS=3 NFA=true EQC=corp/$n
  mkdir res/$n-10
  cp benchmark-results.txt res/$n-10/results.txt
  cp benchmark.log res/$n-10/results.log

  make bench -B NAME=corp/$n PTK=10 WORDS=3 NFA=true EQC=corp/${n}_2
  mkdir res/$n-10-2
  cp benchmark-results.txt res/$n-10-2/results.txt
  cp benchmark.log res/$n-10-2/results.log

  make bench -B NAME=corp/$n PTK=15 WORDS=3 NFA=true EQC=corp/$n
  mkdir res/$n-15
  cp benchmark-results.txt res/$n-15/results.txt
  cp benchmark.log res/$n-15/results.log

  make bench -B NAME=corp/$n PTK=15 WORDS=3 NFA=true EQC=corp/${n}_2
  mkdir res/$n-15-2
  cp benchmark-results.txt res/$n-15-2/results.txt
  cp benchmark.log res/$n-15-2/results.log

  make bench -B NAME=corp/$n PTK=20 WORDS=3 NFA=true EQC=corp/$n
  mkdir res/$n-20
  cp benchmark-results.txt res/$n-20/results.txt
  cp benchmark.log res/$n-20/results.log

  make bench -B NAME=corp/$n PTK=20 WORDS=3 NFA=true EQC=corp/${n}_2
  mkdir res/$n-20-2
  cp benchmark-results.txt res/$n-20-2/results.txt
  cp benchmark.log res/$n-20-2/results.log
done < corp/bench_no_nfa_weak.list

while read -r n; do
  make bench -B NAME=corp/$n PTK=5 WORDS=3 EQC=corp/$n
  mkdir res/$n-nfa-05
  cp benchmark-results.txt res/$n-nfa-05/results.txt
  cp benchmark.log res/$n-nfa-05/results.log

  make bench -B NAME=corp/$n PTK=5 WORDS=3 EQC=corp/${n}_2
  mkdir res/$n-nfa-05-2
  cp benchmark-results.txt res/$n-nfa-05-2/results.txt
  cp benchmark.log res/$n-nfa-05-2/results.log

  make bench -B NAME=corp/$n PTK=10 WORDS=3 EQC=corp/$n
  mkdir res/$n-nfa-10
  cp benchmark-results.txt res/$n-nfa-10/results.txt
  cp benchmark.log res/$n-nfa-10/results.log

  make bench -B NAME=corp/$n PTK=10 WORDS=3 EQC=corp/${n}_2
  mkdir res/$n-nfa-10-2
  cp benchmark-results.txt res/$n-nfa-10-2/results.txt
  cp benchmark.log res/$n-nfa-10-2/results.log
done < corp/bench_nfa.list
