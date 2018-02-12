%% -*- prolog -*-

:- module(kb, [dlrn_status_url/3, github/2, gitrepo/2]).

dlrn_status_url("systemd", "master", "http://38.145.33.116/systemd-master/").
dlrn_status_url("ansible", "devel", "http://38.145.33.116/ansible-devel/").

github("systemd", "https://github.com/systemd/systemd").
github("ansible", "https://github.com/ansible/ansible").

gitrepo("ansible-distgit", "ssh://fedora@38.145.33.116:3300/home/fedora/git/ansible").
gitrepo("systemd-distgit", "ssh://fedora@38.145.33.116:3300/home/fedora/git/systemd").

%% kb.pl ends here
