;;; anvil-test.el --- Tests for anvil.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic smoke tests for the anvil package.

;;; Code:

(require 'ert)
(require 'anvil)

(ert-deftest anvil-test-feature-provided ()
  "Verify that anvil feature is provided."
  (should (featurep 'anvil)))

(ert-deftest anvil-test-customization-group ()
  "Verify customization variables exist."
  (should (boundp 'anvil-modules))
  (should (boundp 'anvil-optional-modules))
  (should (boundp 'anvil-server-id)))

(ert-deftest anvil-test-initial-state ()
  "Verify initial state is disabled."
  (should-not anvil--enabled)
  (should-not anvil--loaded-modules))

(ert-deftest anvil-test-describe-setup-command ()
  "Verify describe-setup is callable."
  (should (fboundp 'anvil-describe-setup)))

;;; anvil-test.el ends here
