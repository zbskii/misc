;; Generate a Hoogle database from all installed packages.
;; Copyright (C) 2010 Chris Done <chrisdone@gmail.com>

;; Usage:
;; (require 'haskell-hoogle-database)
;; (haskell-hoogle-database-update)

;; Running this function the first time will take a while to
;; extract and generate Hoogle databases for all the packages on
;; your system, but subsequent calls are cached and change-aware,
;; and thus miniscule.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

(defun haskell-hoogle-database-refresh ()
  "Generate a Hoogle database from all installed Haskell packages."
  )