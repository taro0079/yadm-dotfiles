HOST="172.27.250.10"
PORT="3306"
USER="root"
PASSWORD="!ChangeMe!"

DATABASE="app_db"
TIMESTAMP=$(date +"%Y%m%d%H%M%S")
DUMP_FILE_NAME="dump_app_db_at_$TIMESTAMP.sql"

LOCALHOST="taro-tobefilledbyoem"

mysqldump -h $HOST -P $PORT -u $USER -p$PASSWORD $DATABASE > $DUMP_FILE_NAME
mysql -h $LOCALHOST -P $PORT -u $USER -p$PASSWORD $DATABASE < $DUMP_FILE_NAME
