#!/usr/bin/env bash
set -euo pipefail

# ===== 設定ここから =====
# 必ず絶対パスを書いた方が安全
SRC_DIR="${SRC_DIR:-/Users/taro_morita/ghq/rpst-v2}"      # 監視したいローカルディレクトリ（フルパス推奨）
DEST_HOST="${DEST_HOST:-taro_morita@dev-tmorita.precs.jp}"    # SSH 接続先
DEST_DIR="${DEST_DIR:-/var/www/rpst-v2/dev}"   # リモート側ディレクトリ

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

# パスを絶対パス化（SRC_DIR が相対で指定された場合に備えて）
SRC_DIR="$(cd "$SRC_DIR" && pwd)"

echo "監視開始: $SRC_DIR -> $DEST_HOST:$DEST_DIR"

# fswatch は変更があったパスを出してくれるので、それを 1 行ずつ読む
# -r: 再帰
# -0: NUL 区切り（空白/日本語ファイル名対策）
fswatch -r -0 "$SRC_DIR" | while IFS= read -r -d '' changed_path; do
  # changed_path 例: /Users/you/dev/project/src/foo.php
  # SRC_DIR からの相対パスに変換
  rel_path="${changed_path#$SRC_DIR/}"

  echo "検知: $rel_path"

  # ファイル/ディレクトリがまだ存在するかで「削除イベントかどうか」を判定
  if [ ! -e "$changed_path" ]; then
    # 削除とみなしてリモートからも削除
    echo " -> リモート削除: $DEST_DIR/$rel_path"
    ssh "$DEST_HOST" "rm -rf \"$DEST_DIR/$rel_path\""
    continue
  fi

  # ここに来るのは「作成 or 更新 or 移動後」の状態
  # ディレクトリかどうか
  if [ -d "$changed_path" ]; then
    is_dir=true
  else
    is_dir=false
  fi

  # rsync で使う「SRC_DIR/./相対パス」形式
  # --relative とセットで使うと、./より後の部分だけを DEST_DIR 配下に再現してくれる
  src_with_root="$SRC_DIR/./$rel_path"

  echo " -> rsync で反映: $rel_path"

  if $is_dir; then
    # ディレクトリの場合、中身ごと送りたいのでスラッシュ付き
    rsync "${COMMON_RSYNC_OPTS[@]}" --relative "$src_with_root/" "$DEST_HOST:$DEST_DIR/"
  else
    rsync "${COMMON_RSYNC_OPTS[@]}" --relative "$src_with_root"   "$DEST_HOST:$DEST_DIR/"
  fi
done
