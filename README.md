# ma321
# MA321 group project

# jp edidite the project5

# Copy SSHkey to github command
$pbcopy < ~/.ssh/id_rsa.pub

# ADD IDENTITY IN CONFIG FILE
$ git config --global user.name "jparep"
$ git config --global user.email parepson@gmail.com

# CHECK LIST OF USERS IN CONFIG FILE
$git config --list


# TO GET UPDATED PROJECT FILES
$git remote -v  # check remote
$git remote add upstream https://github.com/jparep/ma321.git
$git remote -v #check whether upstream added
$git fetch upstream # get the updated ones from origon
$git checkout main #go to main branch
$git merge upstrea/main #merge project from origin to yours to synch
