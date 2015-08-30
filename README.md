# cl-portroach

Common Lisp library to interact with [OpenBSD Portroach](http://portroach.openbsd.org/).

Still very much a WIP and lacks documentation, but partly works quite fine:

    CL-USER> (portroach:new-ports-for "jasper")
    (("devel/libffi" "3.2.1") ("devel/msp430/msp430mcu" "")
     ("devel/ruby-rspec/serverspec" "2.22.0") ("devel/ruby-rspec/specinfra" "2.42.2")
     ("sysutils/logstash/logstash" "1.5.4"))
    CL-USER>

## Examples

These may not actually work, but serve as a mental `*scratch*` buffer
of how `cl-portroach` should function.

	(update-cache) => updates the cache
	(clean-cache) => purges the cache

	(summary :field "total_ports") => 7517
	(print-summary) => pretty printed summary

	(print-maintainers) => list of all maintainers, including their stats

	(ports-for name) => get all ports for the maintainer
	(new-ports-for name) get all new ports for the maintainer in a list of pairs: (pkgpath . newver)

	(describe-maintainer name :field "percentage") => percentage for the given maintainer (searches on name)
	(describe-maintainer email@address :field "percentage") => like above, but search on email due to '@' sign

## Cache

In order not to make a network request upon every single function
call, `make-request` checks it's cache first for a given `file`.
The cache can be updated with `update-cache` or purged with `clean-cache`.

## Viewing headers

	(setf drakma:*header-stream* *standard-output*)
