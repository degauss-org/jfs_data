build:
	docker build -t jfs_data .

shell:
	docker run --rm -it -p 127.0.0.1:3838:3838 --entrypoint=/bin/bash jfs_data

run:
	open "http://127.0.0.1:3838" & docker run --rm -p 3838:3838 jfs_data 
	
release:
ifndef VERSION
	$(error VERSION is not set. Usage: "make release VERSION=X.X")
endif
ifndef DOCKER_USERNAME
	$(error DOCKER_USERNAME is not set)
endif
ifndef DOCKER_PAT
	$(error DOCKER_PAT is not set)
endif
	git commit -am "Release for image version $(VERSION)" --allow-empty
	git tag -fa $(VERSION) -m "${VERSION}"
	git push origin ${VERSION}
	git push
	echo "${DOCKER_PAT}" | docker login -u "${DOCKER_USERNAME}" --password-stdin
	docker tag ${IMAGE}:latest ${IMAGE}:${VERSION}
	docker push ${IMAGE}:${VERSION}
	docker push ${IMAGE}:latest