Feature: Async
  As an Ecukes user
  I want to be able to define async step deefinitions

  Scenario: Callbacked
    Given feature "async":
      """
      Feature: Sleep for
        Scenario: Ten seconds
          Given I sleep for '1 second
      """
    Given step definition:
      """
      (define-step-async "I sleep for `seconds' second"
        (sleep-for (string-to-number seconds))
        (funcall callback))
      """
    When I run ecukes "features/async.feature --reporter dot"
    Then I should see command output:
      """
      .

      1 scenarios (0 failed, 1 passed)
      1 steps (0 failed, 0 skipped, 1 passed)
      """

  Scenario: Byte compiled step definitions file
    Given feature "async":
      """
      Feature: Sleep for
        Scenario: Ten seconds
          Given I sleep for "0" second
      """
    And step definition:
      """
      (define-step-async "I sleep for `seconds' second"
        (funcall callback))
      """
    When I byte compile "features/step-definitions/super-project-steps.el"
    And I run ecukes "features/async.feature --reporter dot"
    Then I should see command output:
      """
      .

      1 scenarios (0 failed, 1 passed)
      1 steps (0 failed, 0 skipped, 1 passed)
      """

  Scenario: Not callbacked
    Given feature "async":
      """
      Feature: Sleep for
        Scenario: Ten seconds
          Given I sleep for '1 seconds
      """
    Given step definition:
      """
      (define-step-async "I sleep for `seconds' seconds"
          ;; not callbacked
          )
      """
    When I run ecukes "features/async.feature --reporter dot --timeout 1"
    Then I should see command error:
      """
      .

        Scenario: Ten seconds
          Given I sleep for '1 seconds
            Did not callback async step within 1 seconds

      1 scenarios (1 failed, 0 passed)
      1 steps (1 failed, 0 skipped, 0 passed)
      """
