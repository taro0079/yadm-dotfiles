#!/usr/bin/env bash
set -euo pipefail

# ===== 設定ここから =====
SRC_DIR="${SRC_DIR:-/Users/taro_morita/ghq/rpst-v2}"              # ローカルのプロジェクトルート
DEST_HOST="${DEST_HOST:-taro_morita@dev-tmorita.precs.jp}"        # SSH 接続先
DEST_DIR="${DEST_DIR:-/var/www/rpst-v2/dev}"                      # リモートのプロジェクトルート

COMMON_RSYNC_OPTS=(
  -az
  --exclude '.git'
  --exclude 'node_modules'
)
# ===== 設定ここまで =====

if ! command -v fswatch >/dev/null 2>&1; then
  echo "fswatch が見つかりません。brew install fswatch してください。" >&2
  exit 1
fi

if ! command -v rsync >/dev/null 2>&1; then
  echo "rsync が見つかりません。rsync をインストールしてください。" >&2
  exit 1
fi

SRC_DIR="$(cd "$SRC_DIR" && pwd)"

echo "監視開始: $SRC_DIR -> $DEST_HOST:$DEST_DIR"

# -r: 再帰, -0: NUL 区切り（空白・日本語ファイル名対策）
fswatch -r -0 "$SRC_DIR" | while IFS= read -r -d '' changed_path; do
  # changed_path 例: /Users/taro_morita/ghq/rpst-v2/src/foo.php
  rel_path="${changed_path#$SRC_DIR/}"  # プロジェクトルートからの相対パス
  echo "検知: $rel_path"

  # ルートディレクトリ自体のイベント（rel_path が空）なら一旦スキップ
  if [ -z "$rel_path" ]; then
    echo " -> プロジェクトルートのイベントなのでスキップ"
    continue
  fi

  # 削除されたかどうか
  if [ ! -e "$changed_path" ]; then
    echo " -> リモート削除: $DEST_DIR/$rel_path"
    ssh "$DEST_HOST" "rm -rf \"$DEST_DIR/$rel_path\""
    continue
  fi

  # ファイル/ディレクトリ判定
  if [ -d "$changed_path" ]; then
    is_dir=true
  else
    is_dir=false
  fi

  if $is_dir; then
    # ディレクトリの場合: 対応するディレクトリを作って、そのディレクトリ配下を同期
    remote_dir="$DEST_DIR/$rel_path"
    echo " -> ディレクトリ rsync: $changed_path -> $remote_dir/"
    ssh "$DEST_HOST" "mkdir -p \"$remote_dir\""
    rsync "${COMMON_RSYNC_OPTS[@]}" "$changed_path/" "$DEST_HOST:$remote_dir/"
  else
    # ファイルの場合: 親ディレクトリを作ってから、そのディレクトリにファイルを同期
    remote_dir="$DEST_DIR/$(dirname "$rel_path")"
    remote_dir="${remote_dir%/.}"  # dirname が '.' の場合の対策
    echo " -> ファイル rsync: $changed_path -> $remote_dir/"
    ssh "$DEST_HOST" "mkdir -p \"$remote_dir\""
    rsync "${COMMON_RSYNC_OPTS[@]}" "$changed_path" "$DEST_HOST:$remote_dir/"
  fi
done
