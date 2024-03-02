clean:
	rm -rf _site

render:
	quarto render

pdf-slides: render
	@echo "Converting slides to PDF..."
	for file in _site/slides/*.html; do \
		echo "Converting $$file to $$(basename $$file .html).pdf"; \
		docker run --rm -t -v `pwd`:/slides ghcr.io/astefanutti/decktape:3.11.0 $$file _pdf/$$(basename $$file .html).pdf; \
	done
