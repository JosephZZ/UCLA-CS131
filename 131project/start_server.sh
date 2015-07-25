for i in "Alford" "Bolden" "Parker" "Powell" "Hamilton"
do
  echo Starting $i
  python myserver.py $i &
done
echo All servers have been started