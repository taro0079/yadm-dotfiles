filein=$1
opt=${2:-Portrait}  # 長い行のときはLandscapeオプションが便利
vim ${filein} -c 'colorscheme default | set number | TOhtml | w tmp.html | qa!'
wkhtmltopdf --page-size B4 -O ${opt} --footer-left "[date] [time] ${filein}" \
  --footer-right "[page]/[topage]" --no-background --margin-top 4 --margin-right 3 \
  --margin-left 4 --margin-bottom 10 tmp.html ${filein}.pdf 
rm -f tmp.html
