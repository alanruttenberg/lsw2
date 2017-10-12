docker-reasoners: 
	cd docker-reasoners ; docker build -t lsw2/reasoners

docker-lsw: docker-reasoners 
	docker build . -t "lsw2/lisp:$(shell git rev-parse --abbrev-ref HEAD)-$(shell git rev-parse HEAD)"

vagrant-lsw:
	vagrant up
	vagrant suspend

install-criu:
	docker run --rm -it --privileged --pid=host boucher/criu-for-mac

build-criu:
	docker build -t boucher/criu-for-mac ~/repos/criu-for-mac/

lsw-checkpoint:	.checkpoint

.container:
	rm -f .checkpoint
	docker run --net=host -id lsw2/lisp /home/lsw/repos/lsw2/bin/lsw > .container
	sleep 20

.checkpoint: .container
	docker checkpoint create `cat .container` cp1
	cp .container .checkpoint

start-lsw-checkpoint: .checkpoint
	docker start `cat .checkpoint` --checkpoint cp1

stop-lsw-checkpoint: .checkpoint
	docker kill `cat .checkpoint`

docker-su:
	screen ~/Library/Containers/com.docker.docker/Data/com.docker.driver.amd64-linux/tty 

# https://stackoverflow.com/questions/4219255/how-do-you-get-the-list-of-targets-in-a-makefile
.PHONY: no_targets__ list
no_targets__:
list:
	sh -c "$(MAKE) -p no_targets__ | awk -F':' '/^[a-zA-Z0-9][^\$$#\/\\t=]*:([^=]|$$)/ {split(\$$1,A,/ /);for(i in A)print A[i]}' | grep -v '__\$$' | grep -v Makefile | grep -v 'make' | sort"
