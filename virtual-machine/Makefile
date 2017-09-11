docker-reasoners: 
	cd docker-reasoners ; docker build -t lsw2/reasoners

docker-lsw: docker-reasoners 
	docker build . -t "lsw2/lisp:$(shell git rev-parse --abbrev-ref HEAD)-$(shell git rev-parse HEAD)"

vagrant-lsw:
	vagrant up
	vagrant suspend
