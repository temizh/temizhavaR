
```
# We assume we are located at /home/temizhava and the package source code is located at ~/temizhavaR
#run as root
sudo mkdir -p /home/temizhava/postgis_data
sudo chown -R 345:345 /home/temizhava/postgis_data 
sudo chmod -R 775 /home/temizhava/postgis_data
```

```
cd ~/temizhavaR/extdata/DB/
docker compose --env-file=.env.dev up
```

