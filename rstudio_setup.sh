#! /bin/bash

# install r
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
sudo add-apt-repository 'deb https://cran.rstudio.com/bin/linux/ubuntu xenial/'
sudo apt-get update
sudo apt-get install r-base r-base-dev

# install rstudio server
sudo apt-get install gdebi-core
wget https://download2.rstudio.org/rstudio-server-1.0.136-amd64.deb
sudo gdebi rstudio-server-1.0.136-amd64.deb

# configure some stuff
sudo sh -c "echo 'auth-required-user-group=rstudio_users' >> /etc/rstudio/rserver.conf"
sudo sh -c "echo 'r-cran-repos=https://cran.rstudio.com/' >> /etc/rstudio/rsession.conf"

sudo addgroup rstudio_users
# TODO: create users and add them to the rstudio_users
# sudo adduser rstudio
# sudo adduser rstudio rstudio_users

# start rstudio
sudo rstudio-server start

# add r libs library so stuff installed in rstudio accessible via rscript
su rstudio
echo 'export R_LIBS="/home/rstudio/R/x86_64-pc-linux-gnu-library/3.2"' >>~/.bashrc
