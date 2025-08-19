;;; eglot-yaml.el --- YAML Language Server protocol extention for Eglot -*- lexical-binding: t; -*-

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

(defgroup eglot-yaml nil
  "YAML Language Server protocol extention for Eglot."
  :prefix "eglot-yaml-"
  :group 'eglot)

(defvar eglot-yaml--file-schema-alist nil
  "Alist mapping file to schema URIs.")

(defcustom eglot-yaml-custom-schema-resolvers
  '(eglot-yaml-kubernetes-schema-resolver)
  "Custom schema providers.
List of functions that take (BUFFER) and return schema-uri or nil."
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

(defun eglot-yaml--get-schemas (server)
  (jsonrpc-request server :yaml/get/jsonSchema (vector (eglot-path-to-uri (buffer-file-name)))))

(defun eglot-yaml--signal-schema-associations (server associations)
  (jsonrpc-notify server :json/schemaAssociations associations))

(defun eglot-yaml--signal-register-custom-schema-request (server)
  (jsonrpc-notify server :yaml/registerCustomSchemaRequest nil))

(cl-defmethod eglot-handle-request
  ((_server eglot-yaml-lsp-server) (_method (eql custom/schema/request))
   document-uri &rest _)
  (eglot-yaml--resolve-schema document-uri))

(defun eglot-yaml--resolve-schema (document-uri)
  "Resolve DOCUMENT-URI schema by `eglot-yaml-custom-schema-resolvers'."
  (when-let* ((buffer (get-file-buffer (eglot-uri-to-path document-uri))))
    (cl-loop for x in eglot-yaml-custom-schema-resolvers
             for schema-uri = (funcall x buffer)
             when schema-uri
             return schema-uri)))

(defun eglot-yaml-show-schema (server)
  "Show current buffer schema."
  (interactive (list (eglot--current-server-or-lose)))
  (message "%s" (eglot-yaml--get-schemas server)))

;;;###autoload
(defun eglot-yaml-set-schema (server schema-uri)
  "Set current buffer SCHEMA-URI.
IF SERVER is nil, only register SCHEMA-URI for future LSP session."
  (interactive (let ((server (eglot--current-server-or-lose)))
                 (list server (eglot-yaml--read-schema server :uri nil))))
  (eglot-yaml--register-file-schema (buffer-file-name) schema-uri)
  (when server
    (eglot-yaml--signal-project-schema-associations server)))

(defun eglot-yaml-select-schema (server)
  "Select current buffer schema by name."
  (interactive (list (eglot--current-server-or-lose)))
  (eglot-yaml-set-schema server (eglot-yaml--read-schema server :name t)))

(defun eglot-yaml--read-schema (server prop require-match)
  (let* ((schemas (cl-loop
                   for x in (cons '(:name "Kubernetes" :uri "kubernetes")
                                  (seq-into (eglot-yaml--get-all-schemas server) 'list))
                   if (plist-get x prop)
                   collect (cons (plist-get x prop) x)))
         (completion-extra-properties
          (list :annotation-function
                (lambda (x) (format " %s" (or (plist-get (assoc-default x schemas) :description) ""))))))
    (completing-read "Select schema: " schemas nil require-match)))

;;;###autoload
(defun eglot-yaml-reset-schema (server)
  "Reset current buffer schema.
IF SERVER is nil, only unregister SCHEMA-URI for future LSP session."
  (interactive (list (eglot--current-server-or-lose)))
  (eglot-yaml--unregister-file-schema (buffer-file-name))
  (when server
    (eglot-yaml--signal-project-schema-associations server)))

(defun eglot-yaml--signal-project-schema-associations (server)
  "Signal schema associations of project to SERVER."
  (eglot-yaml--signal-schema-associations
   server
   (eglot-yaml--collect-project-schema-associations (eglot--project server))))

(defun eglot-yaml--register-file-schema (file schema-uri)
  "Register FILE and SCHEMA-URI pair to `eglot-yaml--file-schema-alist'."
  (let* ((absname (expand-file-name file)))
    (setf (alist-get absname eglot-yaml--file-schema-alist nil nil #'equal)
          schema-uri)))

(defun eglot-yaml--unregister-file-schema (file)
  "Unregister FILE from `eglot-yaml--file-schema-alist'."
  (setq eglot-yaml--file-schema-alist
        (assoc-delete-all (expand-file-name file) eglot-yaml--file-schema-alist)))

(defun eglot-yaml--collect-project-schema-associations (project)
  "Collect schema associations of PROJECT.

Return value is a plist of the form:
\(:SCHEMA-URI1 [\"glob\" ...] :SCHEMA-URI2 [\"glob\" ...] ...)"
  (let ((root (expand-file-name (project-root project)))
        associations)
    (pcase-dolist (`(,file . ,schema-uri) eglot-yaml--file-schema-alist)
      (when-let* ((relname (and (string-prefix-p root file) (file-relative-name file root)))
                  (glob (concat "/" relname))
                  (schema-prop (intern (concat ":" schema-uri))))
        (setq associations (plist-put associations schema-prop
                                      (vconcat (list glob) (plist-get associations schema-prop))))))
    associations))


;; TODO: skip when multi schema yaml
(defun eglot-yaml-kubernetes-schema-resolver (buffer)
  "Resolve kubernetes schmema for BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (without-restriction
        (goto-char (point-min))
        ;; https://github.com/yannh/kubeconform?tab=readme-ov-file#overriding-schemas-location
        (when-let* ((api-version (save-excursion (and (re-search-forward "^apiVersion:[ \t]*\\([^ \t\n]+\\).*$" nil t) (match-string 1))))
                    (kind (save-excursion (and (re-search-forward "^kind:[ \t]*\\([^ \t\n]+\\).*$" nil t) (match-string 1)))))
          (if (not (string-match-p "\\." api-version))
              ;; Kubernetes: https://raw.githubusercontent.com/yannh/kubernetes-json-schema/master/{{.NormalizedKubernetesVersion}}-standalone{{.StrictSuffix}}/{{.ResourceKind}}{{.KindSuffix}}.json
              (format "%s/%s-%s.json"
                      eglot-yaml-kubernetes-schema-base-url
                      (downcase kind)
                      (string-replace "/" "-" (downcase api-version)))
            ;; CRD: https://raw.githubusercontent.com/datreeio/CRDs-catalog/main/{{.Group}}/{{.ResourceKind}}_{{.ResourceAPIVersion}}.json
            (pcase-let ((`(,group ,version) (string-split api-version "/")))
              (format "%s/%s/%s_%s.json"
                      eglot-yaml-kubernetes-crds-schema-base-url
                      (downcase group) (downcase kind) (downcase version)))))))))


(cl-defgeneric eglot-yaml--after-connect (_server)
  "Hook funtion to run after connecting to SERVER."
  nil)

(cl-defmethod eglot-yaml--after-connect ((server eglot-yaml-lsp-server))
  "Hook funtion to run after connecting to SERVER."
  (eglot-yaml--signal-project-schema-associations server)
  (eglot-yaml--signal-register-custom-schema-request server))

(add-hook 'eglot-connect-hook #'eglot-yaml--after-connect)

(provide 'eglot-yaml)
;;; eglot-yaml.el ends here
