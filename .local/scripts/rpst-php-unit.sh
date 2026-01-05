#!/bin/bash
# set -euo pipefail
if [[ $# -ge 1 ]]; then
  filename="$1"
else
 IFS= read -r filename
 : "${filename:?Usage: $0 <relative_test_file>}"
fi
remote_host="dev-tmorita"
user="taro_morita"
remote_root="/var/www/rpst-v2/dev"
phpunit="$remote_root/vendor/bin/phpunit"
config="$remote_root/tests/app/phpunit/v9/phpunit.xml.dist"
test_target_path=$(printf "/var/www/rpst-v2/dev/%s" "$filename")
ssh -q "${user}@${remote_host}" -- php "$phpunit" -c "$config" "$test_target_path"
# cat /tmp/rpst-php-unit
