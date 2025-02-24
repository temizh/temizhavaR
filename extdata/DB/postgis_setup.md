## Preparing to bring the DB container up
```
# We assume we are located at /home/temizhava and the package source code is located at ~/temizhavaR
#run as root
sudo mkdir -p /home/temizhava/postgis_data
sudo chown -R 345:345 /home/temizhava/postgis_data 
sudo chmod -R 775 /home/temizhava/postgis_data
```

## Bring the DB container up
```
cd ~/temizhavaR/extdata/DB/
docker compose --env-file=.env.dev up
```

## Init the DB (to be run only once)
To create the DB, users. And to give user privileges.
```
./temizhava-db.init.sh
```

## Connect from the host machine as linux user postgres (no pwd required)
```
docker exec -u postgres -it temizhava-postgis psql
```

## Connect from the host machine as linux user postgres into DB temizhava (no pwd required)
```
docker exec -u postgres -it temizhava-postgis psql -d temizhava
```

## Connect from the host machine as DB user havakalitesi (force pwd ask)
```
docker exec -u postgres -it temizhava-postgis psql -U havakalitesi -d temizhava -W
```

## Migrate the SQLite DB into postgresql DB
sqlite_2_postgresql.R

## Connect to postgresql DB and run some basic queries
read_from_db.R
