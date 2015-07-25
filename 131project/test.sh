./start_server.sh

sleep 2
{
	sleep 1
	echo IAMAT egger +37.322752-122.030836 1401072205.798801
} | telnet localhost 13580

pkill -f 'python myserver.py Powell'

{
	sleep 1
	echo IAMAT bergger +34.151324-118.028232 1401496386.27158
} | telnet localhost 13580

pkill -f 'python myserver.py Parker'

{
	sleep 1
	echo IAMAT smallbergger +34.751324-118.928232 1401496586.27158
} | telnet localhost 13581

{
	sleep 1
	echo WHATSAT egger 40 5
} | telnet localhost 13584

pkill -f 'python myserver.py Alford'
pkill -f 'python myserver.py Bolden'
pkill -f 'python myserver.py Hamilton'