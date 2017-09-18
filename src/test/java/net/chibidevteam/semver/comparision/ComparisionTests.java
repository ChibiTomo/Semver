package net.chibidevteam.semver.comparision;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

@RunWith(Suite.class)
@SuiteClasses({ VersionTest.class, ConstraintTest.class, ConstraintHelperTest.class })
public class ComparisionTests {
}
