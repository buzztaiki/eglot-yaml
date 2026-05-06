;;; eglot-yaml.el --- YAML Language Server protocol extension for Eglot -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Taiki Sugawara

;; Author: Taiki Sugawara <buzz.taiki@gmail.com>
;; URL: https://github.com/buzztaiki/eglot-yaml
;; Package-Requires: ((emacs "30.1") (eglot "1.18"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO

;;; Code:

(require 'eglot)
(require 'map)
(require 'url-http)

(defgroup eglot-yaml nil
  "YAML Language Server protocol extension for Eglot."
  :prefix "eglot-yaml-"
  :group 'eglot)

(defvar eglot-yaml--kubernetes-schema-existence-cache (make-hash-table :test 'equal)
  "Cache for Kubernetes schema existence.")

(defvar-local eglot-yaml--override-schema-uri nil
  "Override schema URI for current buffer.")

(defcustom eglot-yaml-schema-associations nil
  "User defined JSON schema associations."
  :type '(repeat (cons (string :tag "Schema URI")
                       (repeat :tag "Globs" string)))
  :set (lambda (symbol value)
         (set-default-toplevel-value symbol value)
         (when (featurep 'eglot-yaml)
           (mapc #'eglot-yaml--signal-schema-associations (eglot-yaml--all-servers)))))

(defcustom eglot-yaml-custom-schema-resolvers
  '(eglot-yaml-resolve-override-schema
    eglot-yaml-resolve-kubernetes-schema)
  "List of custom schema resolver functions.
Each function takes no arguments and operates on the document buffer, and should return a schema URI or nil."
  :type '(repeat function))

(defcustom eglot-yaml-kubernetes-schema-base-url
  "https://raw.githubusercontent.com/yannh/kubernetes-json-schema/master/v1.33.4-standalone-strict"
  "Kubernetes schema catalog base URL."
  :type 'string)

(defcustom eglot-yaml-kubernetes-crds-schema-base-url
  "https://raw.githubusercontent.com/datreeio/CRDs-catalog/main"
  "Kubernetes CRDs schema catalog base URL."
  :type 'string)


;;;###autoload
(defclass eglot-yaml-lsp-server (eglot-lsp-server) ()
  :documentation "YAML language server.")


(defun eglot-yaml--get-all-schemas (server)
  (jsonrpc-request server :yaml/get/all/jsonSchemas (vector (eglot-path-to-uri (buffer-file-name)))))

(defun eglot-yaml--get-schema (server)
  (seq-first (jsonrpc-request server :yaml/get/jsonSchema (vector (eglot-path-to-uri (buffer-file-name))))))

(defun eglot-yaml--signal-schema-associations (server)
  (jsonrpc-notify server :json/schemaAssociations (vector (eglot-yaml--schema-associations))))

(defun eglot-yaml--signal-register-custom-schema-request (server)
  (jsonrpc-notify server :yaml/registerCustomSchemaRequest nil))

(cl-defmethod eglot-handle-request
  ((_server eglot-yaml-lsp-server) (_method (eql custom/schema/request))
   document-uri &rest _)
  (eglot-yaml--resolve-schema document-uri))


(defun eglot-yaml-show-schema (server)
  "Show current buffer schema."
  (interactive (list (eglot--current-server-or-lose)))
  (message "%s" (eglot-yaml--get-schema server)))

(defun eglot-yaml-set-schema (server schema-uri)
  "Set current buffer SCHEMA-URI. "
  (interactive (let* ((server (eglot--current-server-or-lose))
                      (current (map-elt (eglot-yaml--get-schema server) :uri)))
                 (list server (read-string "Schema URI: " current))))
  (setq eglot-yaml--override-schema-uri schema-uri)
  (eglot-signal-didChangeConfiguration server))

(defun eglot-yaml-select-schema (server)
  "Select current buffer schema by name."
  (interactive (list (eglot--current-server-or-lose)))
  (eglot-yaml-set-schema server (eglot-yaml--read-schema server)))

(defun eglot-yaml--read-schema (server)
  (let* ((schemas (cl-loop
                   for schema in (cons '(:name "Kubernetes" :uri "kubernetes")
                                       (seq-into (eglot-yaml--get-all-schemas server) 'list))
                   for name = (map-elt schema :name)
                   when name collect (cons name schema)))
         (completion-extra-properties
          (list :annotation-function
                (lambda (name) (format " %s" (map-nested-elt schemas (list name :description) "")))))
         (name (completing-read "Select schema: " schemas nil t)))
    (map-nested-elt schemas (list name :uri))))

(defun eglot-yaml-reset-schema (server)
  "Reset current buffer schema."
  (interactive (list (eglot--current-server-or-lose)))
  (setq eglot-yaml--override-schema-uri nil)
  (eglot-signal-didChangeConfiguration server))

(defun eglot-yaml--schema-associations ()
  "Get schema associations for json/schemaAssociations notification."
  (seq-mapcat
   (pcase-lambda (`(,schema-uri . ,globs))
     (list (intern (concat ":" schema-uri)) (apply #'vector globs)))
   eglot-yaml-schema-associations))

(defun eglot-yaml--resolve-schema (document-uri)
  "Resolve DOCUMENT-URI schema by `eglot-yaml-custom-schema-resolvers'."
  (when-let* ((buffer (get-file-buffer (eglot-uri-to-path document-uri))))
    (cl-loop for x in eglot-yaml-custom-schema-resolvers
             for schema-uri = (with-current-buffer buffer
                                (save-excursion
                                  (save-restriction
                                    (widen)
                                    (funcall x))))
             when schema-uri
             return schema-uri)))

(defun eglot-yaml-resolve-override-schema ()
  "Resolve override schema for current buffer."
  eglot-yaml--override-schema-uri)

(defun eglot-yaml-resolve-kubernetes-schema ()
  "Resolve kubernetes schema for current buffer."
  ;; limited support for multi schema yaml
  (goto-char (or (re-search-backward "^---" nil t) (point-min)))

  ;; see https://github.com/yannh/kubeconform#overriding-schemas-location
  (when-let* ((api-version (save-excursion (and (re-search-forward "^apiVersion:[ \t]*\\([^ \t\n]+\\).*" nil t) (match-string 1))))
              (kind (save-excursion (and (re-search-forward "^kind:[ \t]*\\([^ \t\n]+\\).*" nil t) (match-string 1)))))
    (let (schema-urls)
      ;; Kubernetes: https://raw.githubusercontent.com/yannh/kubernetes-json-schema/master/{{.NormalizedKubernetesVersion}}-standalone{{.StrictSuffix}}/{{.ResourceKind}}{{.KindSuffix}}.json
      (push (format "%s/%s-%s.json" eglot-yaml-kubernetes-schema-base-url
                    (downcase kind)
                    (string-replace "/" "-" (string-replace ".k8s.io" "" api-version)))
            schema-urls)
      (pcase (string-split api-version "/")
        (`(,group ,version)
         ;; CRD: https://raw.githubusercontent.com/datreeio/CRDs-catalog/main/{{.Group}}/{{.ResourceKind}}_{{.ResourceAPIVersion}}.json
         (push (format "%s/%s/%s_%s.json" eglot-yaml-kubernetes-crds-schema-base-url
                       (downcase group) (downcase kind) (downcase version))
               schema-urls)))
      (seq-find #'eglot-yaml--kubernetes-schema-exists-p (nreverse schema-urls)))))

(defun eglot-yaml--kubernetes-schema-exists-p (url)
  "Check if Kubernetes schema URL exists, with caching."
  (unless (map-contains-key eglot-yaml--kubernetes-schema-existence-cache url)
    (setf (map-elt eglot-yaml--kubernetes-schema-existence-cache url)
          (let ((url-show-status nil))
            (url-http-file-exists-p url))))
  (map-elt eglot-yaml--kubernetes-schema-existence-cache url))

(defun eglot-yaml--all-servers ()
  "Get all connected `eglot-yaml-lsp-server' instances."
  (cl-loop for servers
           being hash-values of eglot--servers-by-project
           append (seq-filter #'eglot-yaml-lsp-server-p servers)))

(defun eglot-yaml--after-connect (server)
  "Hook function to run after connecting to SERVER."
  (when (eglot-yaml-lsp-server-p server)
    (eglot-yaml--signal-schema-associations server)
    (eglot-yaml--signal-register-custom-schema-request server)))

(add-hook 'eglot-connect-hook #'eglot-yaml--after-connect)

(provide 'eglot-yaml)
;;; eglot-yaml.el ends here
