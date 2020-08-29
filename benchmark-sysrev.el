(with-delay 0.5 (garbage-collect))
(with-delay 2.0 (--benchmark-sysrev))
;;(with-delay 4.0 (kill-emacs))
