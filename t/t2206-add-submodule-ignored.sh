#!/bin/sh

test_description='git add respects submodule ignore=all and explicit pathspec'

. ./test-lib.sh

GIT_TEST_DEFAULT_INITIAL_BRANCH_NAME=main
export GIT_TEST_DEFAULT_INITIAL_BRANCH_NAME

base_path=$(pwd -P)

#1
test_expect_success 'setup: create origin repos'  '
	cd "${base_path}" &&
	git config --global protocol.file.allow always &&
	git init sub && 
		pwd &&
		cd sub &&
		test_commit sub_file1 &&
		git tag v1.0 &&
		test_commit sub_file2 &&
		git tag v2.0 &&
	cd "${base_path}" &&
	git init main &&
		cd main &&
		test_commit first &&
	cd "${base_path}"
'
#2
test_expect_success 'main: add submodule and config ignore=all'  '
	cd "${base_path}" &&
	cd main && 
	git submodule add ../sub &&
	git commit -m "add submodule" &&
	git config -f .gitmodules submodule.sub.ignore all &&
	git add . &&
	git commit -m "update submodule config sub.ignore all" &&
	! git status --porcelain | grep "^.*$" &&
	echo
'

#3
test_expect_success 'sub: change to different sha1 and check status in main'  '
	cd "${base_path}" &&
	cd main &&
	git -C sub reset --hard v1.0 &&
	! git status --porcelain | grep "^ M sub$" &&
	git status --ignore-submodules=none --porcelain | grep "^ M sub$" &&
	echo
'

#4
test_expect_success 'main: check normal add and status'  '
	cd "${base_path}" &&
	cd main &&
	git add . &&
	! git status --porcelain | grep "^ M sub$" &&
	echo
'

#5
test_expect_success 'main: check force add and status'  '
	cd "${base_path}" &&
	cd main &&
	git add --force . &&
	git status --porcelain | grep "^M  sub$" &&
	git commit -m "update submodule pointer" &&
	! git status --porcelain | grep "^ M sub$" &&
	git log --ignore-submodules=none --name-only --oneline | grep "^sub$" &&
	echo
'
test_done
exit 0
