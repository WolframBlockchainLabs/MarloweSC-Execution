#!/bin/sh

chown -R app:app /usr/src/app/Delphos/
chown -R app:app /usr/src/app/logs/
exec su -s /bin/sh -c "$*" app