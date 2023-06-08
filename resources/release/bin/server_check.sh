SERVER_NAME=easyTrade-platform
PID=$(ps -ef | grep $SERVER_NAME | grep -v grep | awk '{ print $2 }')

if [ -z "$PID" ]
    then
    echo 'restart easyTrade-platform server'
    /home/easytrade/bin/server.sh start
    sleep 10
else
   echo "Heartbeat detection success"
fi
