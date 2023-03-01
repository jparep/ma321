## BAsic GIT COMMANDS TO SETUO NAD CONNECT MA321 GROUPPROJECT

# Copy SSHkey to github command
$pbcopy < ~/.ssh/id_rsa.pub

#ADD IDENTITY IN CONFIG FILE
$ git config --global user.name "jparep"
$ git config --global user.email parepson@gmail.com

# CHECK LIST OF USERS IN CONFIG FILE
$git config --list


## TO GET UPDATED PROJECT FILES
# check remote
$git remote -v  
$git remote add upstream https://github.com/jparep/ma321.git
#check whether upstream added
$git remote -v 
# get the updated ones from origon
$git fetch upstream 
#go to main branch
$git checkout main 
#merge project from origin to yours to synch
$git merge upstrea/main  

# ISSUE WITH REMOTE
# remove and Add new remote
$git remote -rm origin   #remote remote origin
#add remote origin
$git remote add origin https:/jparep/ma321.git 

## GIT PUSH ISSUE
# push to github with url
$git push git@jp:joshuaparep/ma321.git
