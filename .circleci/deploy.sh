git config --global user.email autodeploy@circleci.com
git config --global user.name CircleCI

git submodule init
git submodule update --remote

stack exec samtay-github-io -- rebuild

cd _site

if [[ $(git status --porcelain) ]]; then
  git add --all
  git commit -m "Update build (`date '+%F %T %Z'`) [ci skip]"
  git push origin master
fi

cd ..

git add _site
if [[ $(git status --porcelain) ]]; then
  git commit -m "Update _site (`date '+%F %T %Z'`) [ci skip]"
  git push origin hakyll
  echo "Deployed site successfully"
else
  echo "No changes to deploy"
fi
