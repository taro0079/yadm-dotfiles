#!/usr/bin/env bash

local_project=$1
remote_project=$2
remote_server=$3

# ローカルのプロジェクトのブランチを取得する
cd $local_project
local_brach=$(git branch --contains | awk '{print $2}')
echo $local_brach
ssh $remote_server "cd $remote_project; git stash ; git checkout .; git fetch -a; git switch $local_brach; git pull"


