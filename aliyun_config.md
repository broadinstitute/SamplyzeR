### Requirement

#### R

```sh
sudo apt-get install libfontconfig1-dev libfreetype6-dev
sudo apt-get install libcurl4-openssl-dev
sudo apt-get install libxml2-dev
sudo apt-get install libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev

install.packages("shiny")
install.packages("ggplot2")
install.packages(c("usethis", "pkgdown", "rcmdcheck", "roxygen2", "rversions", "urlchecker"))
install.packages("ragg")
install.packages("prettyGraphs")
install.packages("devtools")
library(devtools)
devtools::install("/home/jinbo/samplyzer")
library(samplyzer)
```

#### Aliyun config

```sh
sudo apt-get update
sudo apt-get install r-base r-base-dev
sudo apt-get install gdebi-core
wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.16.958-amd64.deb
sudo gdebi shiny-server-1.5.16.958-amd64.deb
sudo ln -s /home/jinbo/samplyzer/inst/apps/samplyzer /srv/shiny-server/samplyzer
```

#### Ngnix config

```
# Install
sudo apt-get update
sudo apt-get install nginx

# /etc/nginx/sites-available/shiny
sudo vim /etc/nginx/sites-available/default  
server {
    listen 80 default_server;
    listen [::]:80 default_server;
    server_name 121.40.162.92;

    location / {
        proxy_pass http://121.40.162.92:3838/samplyzer/;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        rewrite ^/$ /samplyzer/ permanent;
    }
}

sudo systemctl restart nginx
```



### Run

```sh
library(shiny)
library(ggplot2)
runApp('/home/jinbo/samplyzer/inst/apps/samplyzer')
```

**shiny-server**

```sh
sudo systemctl restart shiny-server
```
