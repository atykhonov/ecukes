Feature: Steps

  Scenario: Missing step
    Given feature "foo":
      """
      Feature: Foo

        Scenario: Bar
          Given a missing step
      """
    When I run ecukes "features/foo.feature"
    Then I should see command output:
      """
      Please implement the following step definitions

      (define-step "a missing step"
        ;; ...
        )
      """

  Scenario: Missing steps
    Given feature "foo":
      """
      Feature: Foo

        Scenario: Bar
          Given a missing step

        Scenario: Baz
          Given another missing step
      """
    When I run ecukes "features/foo.feature"
    Then I should see command output:
      """
      Please implement the following step definitions

      (define-step "a missing step"
        ;; ...
        )

      (define-step "another missing step"
        ;; ...
        )
      """

  Scenario: Missing step with argument
    Given feature "foo":
      """
      Feature: Foo

        Scenario: Bar
          Given some "thing"
      """
    When I run ecukes "features/foo.feature"
    Then I should see command output:
      """
      Please implement the following step definitions

      (define-step "some `arg-1'"
        ;; ...
        )
      """

  Scenario: Missing step with arguments
    Given feature "foo":
      """
      Feature: Foo

        Scenario: Bar
          Given some "thing" and some "one"
      """
    When I run ecukes "features/foo.feature"
    Then I should see command output:
      """
      Please implement the following step definitions

      (define-step "some `arg-1' and some `arg-2'"
        ;; ...
        )
      """
