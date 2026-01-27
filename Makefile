# Define directories
PYTHON_SRC_DIR = python
R_SRC_DIR = R
DOCS_SRC_DIR = docs
DATA_DIR = data

# Define targets
PYTHON_SCRIPTS = $(wildcard $(PYTHON_SRC_DIR)/*.py)
R_SCRIPTS = $(wildcard $(R_SRC_DIR)/*.R)
QUARTO_DOCS = $(wildcard $(DOCS_SRC_DIR)/*.qmd)
HTML_FILES = $(patsubst %.qmd, %.html, $(QUARTO_DOCS))
RUN_R_FLAG = run_R_done.flag

$(info ************************************)
$(info python source directory:   $(PYTHON_SRC_DIR))
$(info R Source director:         $(R_SRC_DIR))
$(info Docs Source director:      $(DOCS_SRC_DIR))
$(info R scripts:                 $(R_SCRIPTS))
$(info Python scripts:            $(PYTHON_SCRIPTS))
$(info Quarto scripts:            $(QUARTO_DOCS))
$(info HTML files:                $(HTML_FILES))
$(info ************************************)

# Default target
all: build_docker run_python run_R render_docs

run_container: R_container docs_container

run_local: run_python run_R render_docs

build_docker:
	@echo "Building Docker image..."
	docker build --tag gcrmn_alt .

build_singularity:
	## sudo mount -o remount,size=16G /tmp
	@echo "Building singularity image..."
	docker save gcrmn_alt -o gcrmn_alt.tar
	apptainer build gcrmn_alt.sif docker-archive://gcrmn_alt.tar

build_singularity2:
	@echo "Modifying singularity image..."
	apptainer build --sandbox gcrmn_alt_sandbox.sif gcrmn_alt.sif
	sudo apptainer shell --writable gcrmn_alt_sandbox.sif \
	 export CXXFLAGS="-Wno-error=changes-meaning -Wno-deprecated-declarations" \
	 export CXX_FLAGS="-Wno-error=changes-meaning -Wno-deprecated-declarations"
	apptainer build gcrmn_alt2.sif gcrmn_alt_sandbox.sif

ssh_singularity:
	@echo "Copying singularity image to HPC..."
	scp gcrmn_alt2.sif mlogan@hpc-l001.aims.gov.au:~/Work/AIMS/GCRMN/gcrmn_model_alt/gcrmn_alt2.sif

docs_container:
	docker run --rm -v "$(shell pwd)":/home/Project gcrmn_alt $(MAKE) render_docs

python_container:
	@echo "Running Python scripts..."
	docker run --rm -v "$(shell pwd)":/home/Project gcrmn_alt $(MAKE) run_python

R_container:
	@echo "Running R targets pipeline..."
	docker run --rm -v "$(shell pwd)":/home/Project gcrmn_alt $(MAKE) run_R

# Rule to run Python scripts
# run_python: $(PYTHON_SCRIPTS)
#		@echo "Running Python scripts..."
#		python3 $^
run_python:
	@echo "Running python ploomber pipeline..."
	cd python && ploomber build

# Rule to run the R targets pipeline
run_R:
	@echo "Running R targets pipeline..."
	cd R && Rscript -e "targets::tar_make()"

slurm_R:
	@echo "Running R targets pipeline via slurm..."
	rm -f analysis_*.log
	rm -f analysis_*.stderr
	sbatch analysis.slurm

slurm_docs:
	@echo "Running R render documents pipeline via slurm..."
	rm -f docs_*.log
	rm -f docs_*.stderr
	sbatch docs.slurm

# %.html: %.qmd $(RUN_R_FLAG) $(PYTHON_SRC_DIR)/%.py
%.html: %.qmd
	@echo "Rendering $< to $@..."
	echo "library(quarto); quarto_render(\"$<\")" | R --no-save --no-restore;
	echo "library(quarto); quarto_render(\"$<\", output_format = 'docx')" | R --no-save --no-restore;

# Rule to render all Quarto documents
render_docs: $(HTML_FILES) $(PYTHON_SCRIPTS) $(R_SCRIPTS)

copy_regions:
	@echo "Creating regions from australia.qmd"
	cat docs/australia.qmd | sed "s/Australia/Brazil/g" > docs/brazil.qmd
	cat docs/australia.qmd | sed "s/Australia/Caribbean/g" > docs/caribbean.qmd
	cat docs/australia.qmd | sed "s/Australia/EAS/g" > docs/eas.qmd
	cat docs/australia.qmd | sed "s/Australia/ETP/g" > docs/etp.qmd
	cat docs/australia.qmd | sed "s/Australia/Pacific/g" > docs/pacific.qmd
	cat docs/australia.qmd | sed "s/Australia/PERSGA/g" > docs/persga.qmd
	cat docs/australia.qmd | sed "s/Australia/ROPME/g" > docs/ropme.qmd
	cat docs/australia.qmd | sed "s/Australia/South Asia/g" > docs/south_asia.qmd
	cat docs/australia.qmd | sed "s/Australia/WIO/g" > docs/wio.qmd

# Clean up intermediate files
clean:
	@echo "Cleaning up..."
	rm -rf _targets/ $(RESULTS_DIR)/*.html $(RESULTS_DIR)/*.pdf $(RUN_R_FLAG)
