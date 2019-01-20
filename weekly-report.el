(provide 'weekly-report)

;#進捗報告: 2019/01/10 - 2019/01/17

(defvar weekly-report-directory-format "/dir/%Y%m%d/")
(defvar weekly-report-file-format "%Y%m%d.txt")
(defvar weekly-report-period-title "#進捗報告: ")
(defvar weekly-report-period-format "%Y%m%d - %Y%m%d")
(defvar weekly-report-period-title-regexp (concat weekly-report-period-title ".+$"))

(defvar one-day-in-seconds (* 24 60 60))

(defun update-weekly-report-period (&optional offset-days)
  "Update weekly report period header in the current buffer."
  (interactive)
  (let
      ((new-title (weekly-report-period-string offset-days)))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward weekly-report-period-title-regexp)
	  (progn
	    (looking-at weekly-report-period-title-regexp)
	    (replace-match (weekly-report-period-string offset-days)))))))

(defun copy-weekly-report ()
  "Copy weekly report to the specified directory."
  (interactive)
  (let* ((dir-file-pair (weekly-report-file (current-time))))
    (progn
      (if (not (file-exists-p (car dir-file-pair)))
	  (error (concat "dir does not exists: " (car dir-file-pair))))
      (write-region
       (point-min) (point-max)
       (concat (car dir-file-pair) (cdr dir-file-pair))))))


(defun weekly-report-period (now &optional offset-days)
  "return weekly report period based on today"
  (let* ((day-of-week (format-time-string "%w" (current-time)))
	(day-offset
	 (cond
	  ((equal "0" day-of-week) -3) ; Sunday
	  ((equal "1" day-of-week) -4) ; Monday
	  ((equal "2" day-of-week) -5) ; Tuesday  
	  ((equal "3" day-of-week) -6) ; Wednesday
	  ((equal "4" day-of-week) 0)  ; Thursday is the day for weekly report.
	  ((equal "5" day-of-week) -1) ; Friday   
	  ((equal "6" day-of-week) -2) ; Saturday 
	  (t 0)))
	(days (+ day-offset (if (null offset-days) 0 offset-days)))
	(last (time-add now (seconds-to-time (* days one-day-in-seconds)))))
    (one-week last)))

(defun one-week (last)
  (cons (time-add last (seconds-to-time (* -7 one-day-in-seconds))) last))

(defun weekly-report-file (time &optional offset-days)
  (let ((period (weekly-report-period time offset-days)))
    (cons
     (format-time-string weekly-report-directory-format (cdr period))
     (format-time-string weekly-report-file-format (cdr period)))))

(defun weekly-report-period-string (&optional offset-days)
  (let ((period (weekly-report-period (current-time) offset-days))
    (format (concat weekly-report-period-title weekly-report-period-format) (car period) (cdr period))))

