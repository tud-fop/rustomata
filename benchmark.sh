
mkdir res/

for f in corp_nfa/*.readable; do
  n=$(basename -s .readable $f)
  make benchmark -B NAME=corp/$n PTK=5 WORDS=10
  cp benchmark-results.txt res/$n-5-nfa-results.txt

  make benchmark -B NAME=corp/$n PTK=10 WORDS=10
  cp benchmark-results.txt res/$n-10-nfa-results.txt
done

for f in corp/*.readable
  n=$(basename -s .readable $f)
  make benchmark -B NAME=corp/$n PTK=5 WORDS=10 NFA=true
  cp benchmark-results.txt res/$n-5-results.txt

  make benchmark -B NAME=corp/$n PTK=10 WORDS=10 NFA=true
  cp benchmark-results.txt res/$n-10-results.txt

  make benchmark -B NAME=corp/$n PTK=20 WORDS=10 NFA=true
  cp benchmark-results.txt res/$n-20-results.txt
done
