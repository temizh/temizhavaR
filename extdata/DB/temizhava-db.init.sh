set -a; source .env; set +a  # Load environment variables

docker exec -i temizhava-postgis psql -U postgres  <<EOF
-- Create DB and superuser

CREATE DATABASE temizhava;
CREATE ROLE temizhavaadmin WITH LOGIN PASSWORD '$POSTGRES_TADMIN_PASSWORD';
ALTER ROLE temizhavaadmin WITH SUPERUSER;
EOF

docker exec -i temizhava-postgis psql -U postgres -d "$TEMIZHAVA_DB" <<EOF
-- Create user


CREATE USER $POSTGRES_TUSER PASSWORD '$POSTGRES_TUSER_PASSWORD';

-- Grant privileges
GRANT ALL PRIVILEGES ON DATABASE $TEMIZHAVA_DB TO $POSTGRES_TUSER;

-- Allow user to create objects in public schema
ALTER SCHEMA public OWNER TO $POSTGRES_TUSER;
GRANT USAGE, CREATE ON SCHEMA public TO $POSTGRES_TUSER;
EOF

