Feature: Define step

  Scenario: Solide text
    Given step definition:
      """
      (define-step "solide text"
        (setq test t))
      """
    Given feature "solide":
      """
      Feature: Solide

        Scenario: Solide text
          Given solide text
      """
    When I run ecukes "features/solide.feature --reporter spec"
    Then I should see command output:
      """
      Feature: Solide

        Scenario: Solide text
          Given solide text

      1 scenarios (0 failed, 1 passed)
      1 steps
      """

  Scenario: Step with variable
    Given step definition:
      """
      (define-step "foo `bar' baz"
        (print (format "TEST BEST REST"))
        (setq ecukes-test-variable "best"))
      """
    Given feature "variable":
      """
      Feature: Single variable
        
        Scenario: Single variable
          Given foo "hello world" baz
      """
    When I run ecukes "features/variable.feature --reporter spec"    
