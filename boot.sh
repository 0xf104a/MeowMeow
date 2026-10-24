#!/bin/sh
# Entry point of the production image (Dockerfile).

release=$(cut -d' ' -f2 /opt/MeowMeow/releases/start_erl.data 2>/dev/null || echo unknown)
echo "=> Booting MeowMeow webserver"
echo "   release ${release}, commit ${MEOWMEOW_COMMIT:-unknown}," \
     "ref ${MEOWMEOW_REF:-unknown}, built ${MEOWMEOW_BUILD_DATE:-unknown}"
exec /opt/MeowMeow/bin/MeowMeow foreground
