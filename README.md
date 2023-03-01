#Packages neededin this project
$tidyverse
$dplyr
$ggplot2

## Basic GIT commands to set up and acces MA321 Group Project

# Check for an exisitng SH key
$ls -al ~/.ssh
# Create file .ssh file directory of doesn't exist
$mkdir $HOME/.ssh

# 1. Generate SSH Key
$ ssh-keygen -t ed25519 -C "parepson@gmail.com"
# 2. start the ssh-agent in the background
$ eval "$(ssh-agent -s)"
# 3. Add private SSH private key to ssh-agent
$ ssh-add ~/.ssh/id_ed25519

# Copy SSHkey to github command
$pbcopy < ~/.ssh/id_rsa.pub

# ADD IDENTITY IN CONFIG FILE
$ git config --global user.name "jparep"
$ git config --global user.email parepson@gmail.com

# CHECK LIST OF USERS IN CONFIG FILE
$git config --list


## TO GET UPDATED PROJECT FILES
# check remote
$git remote -v  
# add url to use ssh key instead https which require github login 
$git remote add upstream git@jparep:jparep/ma321.git
#check whether upstream added
$git remote -v 
# get the updated ones from origon only fork projects
$git fetch upstream 
# go to main branch
$git checkout main 
# merge project from origin to yours to sync
# merge only for fork projects
$git merge upstrea/main 
# merge for origin before working on the project
$git merge origin 

# ISSUE WITH REMOTE
# remove and Add new remote
$git remote remove origin
# add remote origin to your local
$git remote add origin git@jparep:jparep/ma321.git
# OR (jparep afet @ is configured as alias in config file)
$git remote set-url origin git@jparep:jparep/ma321.git

## GIT PUSH ISSUE
# push to github with url for one time only
$git push git@jp:joshuaparep/ma321.git
# After that git push without url for latter push
$git push
#git force to push if git push issue
$git push -f origin main

# check status of changes if made in the file
$git status
$git diff

