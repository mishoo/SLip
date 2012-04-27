server {
       	listen 80;
	server_name ss-lisp-test.local;
	charset utf-8;

	location / {
		root /home/mishoo/LC/;
                dav_methods PUT DELETE MKCOL COPY MOVE;
                create_full_put_path on;
                dav_access group:rw all:r;
		autoindex on;
	}
}
