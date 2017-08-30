FROM selenium/standalone-firefox:3.4.0
MAINTAINER "Christoph Fabianek" christoph@ownyourdata.eu

USER root

RUN apt-get update \ 
	&& apt-get install -y --no-install-recommends \
		ed \
		less \
		locales \
		vim-tiny \
		wget \
		ca-certificates \
		fonts-texgyre \
		libssl-dev \
		libcurl4-openssl-dev \
		libssh2-1-dev \
		phantomjs \
		libxml2-dev \
		netcat \
	&& rm -rf /var/lib/apt/lists/*

## Configure default locale, see https://github.com/rocker-org/rocker/issues/19
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
	&& locale-gen en_US.utf8 \
	&& /usr/sbin/update-locale LANG=en_US.UTF-8

ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8

RUN echo "deb http://cloud.r-project.org/bin/linux/ubuntu xenial/" >> /etc/apt/sources.list \
	&& apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9 \
	&& apt-get update \
	&& apt-get install -y --no-install-recommends \
		r-base \
		r-base-dev \
	&& R -e "install.packages(c('jsonlite', 'devtools'), repos='http://cran.r-project.org')" \
	&& R -e "devtools::install_github('ropensci/RSelenium')" \
	&& rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
	&& mkdir -p /srv-bank

USER seluser

COPY script/run.sh /bin/run.sh
COPY script/autoLoad.R /srv-bank/autoLoad.R

EXPOSE 4444

CMD ["/bin/run.sh"]