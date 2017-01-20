
;;(ensime-mode +1)
;; helm integration with ensime
(setq ensime-use-helm t)

;; SBT
;;
;; SBT need to be on the path, otherwise do
;; (add-to-list 'exec-path "/usr/local/bin")
;; create a gitignore_global and add ".ensime"
;; do git config --global core.excludesfile ~/.gitignore_global

;; maven
;; 
;; 1 generate a .ensime file
;;  1-1 with maven
;; add ~/.m2/settings.xml and put
;;<settings xmlns="http://maven.apache.org/SETTINGS/1.0.0"
;;  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
;;  xsi:schemaLocation="http://maven.apache.org/SETTINGS/1.0.0
;;                      https://maven.apache.org/xsd/settings-1.0.0.xsd">
;;
;;  <pluginGroups>
;;    <pluginGroup>org.ensime.maven.plugins</pluginGroup>
;;  </pluginGroups>
;;</settings>
;;
;; then
;; resolve dependency and generate ensim file
;; mvn dependency:sources dependency:resolve -Dclassifier=javadoc
;; mvn ensime:generate
;;
;;
;;  1-2 with sbt
;; add these lines to ~/.sbt/0.13/global.sbt
;; import org.ensime.EnsimeCoursierKeys._
;; import org.ensime.EnsimeKeys._
;;
;; ensimeServerVersion in ThisBuild := "2.0.0-SNAPSHOT"
;; ensimeIgnoreMissingDirectories := true

;; then to  ~/.sbt/0.13/plugins/plugins.sbt
;; addSbtPlugin("org.ensime" % "sbt-ensime" % "1.12.5") // ensime developers should use 1.9.0
;;
;; then use the following sbt commands
;;ensimeConfig — Generate a .ensime for the project (takes space-separated parameters to restrict to subprojects).
;;ensimeConfigProject — Generate a project/.ensime for the project definition.
;;debugging — Add debugging flags to all forked JVM processes.
;;debuggingOff — Remove debugging flags from all forked JVM processes.

;; 2 start ensime M-x ensime

(setq ensime-startup-snapshot-notification nil)
(setq ensime-startup-notification nil)


;;(defcustom ensime-goto-test-config-amadeus
;;   '(:test-class-names-fn ensime-goto-test--test-class-names
;;    :test-class-suffixes  ("Test" "Spec" "Specification" "Check")
;;    :impl-class-name-fn ensime-goto-test--impl-class-name
;;    :impl-to-test-dir-fn ensime-goto-test--impl-to-test-dir
;;    :is-test-dir-fn ensime-goto-test--is-test-dir-amadeus
;;    :test-template-fn ensime-goto-test--test-template-amadeus-spark)
;;   "Configuration for amadeus projects "
;;    :type 'plist
;;    :group 'ensime-ui)
