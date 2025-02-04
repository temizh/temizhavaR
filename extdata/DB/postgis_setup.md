
```
# We assume we are located at /home/temizhava and the package source code is located at ~/temizhavaR
mkdir -p /home/temizhava/postgis_data
sudo chown -R 345:345 /home/temizhava/postgis_data #run as root
```

```
cd ~/temizhavaR/extdata/DB/
docker compose --env-file=.env.dev up
```

