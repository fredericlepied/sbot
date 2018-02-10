%% -*- prolog -*-

:- module(kb, [dlrn_status_url/3, github/2]).

dlrn_status_url('systemd', 'master', 'http://38.145.33.116/systemd-master/report.html').
dlrn_status_url('ansible', 'devel', 'http://38.145.33.116/ansible-devel/report.html').

github('systemd', 'https://github.com/systemd/systemd').
github('ansible', 'https://github.com/ansible/ansible').

%% kb.pl ends here
