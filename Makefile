run: build
	docker run -v `pwd`:/workdir -it csg-tokyo/typelevellr:latest \
		./docker-entrypoint.sh
test: build
	docker run -t -v `pwd`:/workdir csg-tokyo/typelevellr:latest \
		stack test --allow-different-user
build:
	docker build -t csg-tokyo/typelevellr .
.PHONY: run test build