# setup_tunnel.sh
# Called by worker_entry.R before any DB connections
#!/bin/bash

mkdir -p ~/.ssh
chmod 700 ~/.ssh

# Write SSH key from environment variable (injected by AWS Secrets Manager)
echo "$SSH_PRIVATE_KEY" > ~/.ssh/data_server_key
chmod 600 ~/.ssh/data_server_key

# Add host key to avoid interactive prompt
ssh-keyscan -H 3.127.165.72 >> ~/.ssh/known_hosts 2>/dev/null

# Start SSH tunnel in background
autossh -M 0 \
  -i ~/.ssh/data_server_key \
  -L 5433:localhost:5432 \
  -N \
  -o "ServerAliveInterval=30" \
  -o "ServerAliveCountMax=3" \
  -o "StrictHostKeyChecking=no" \
  aarogers@3.127.165.72 &

TUNNEL_PID=$!
echo $TUNNEL_PID > /tmp/tunnel.pid

# Wait for tunnel to be ready
for i in $(seq 1 30); do
  if nc -z 127.0.0.1 5433 2>/dev/null; then
    echo "SSH tunnel ready after ${i}s"
    exit 0
  fi
  sleep 1
done

echo "ERROR: SSH tunnel failed to establish"
exit 1