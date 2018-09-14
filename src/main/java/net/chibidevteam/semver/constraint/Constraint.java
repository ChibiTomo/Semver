package net.chibidevteam.semver.constraint;

import java.util.ArrayList;
import java.util.List;

import lombok.Value;
import net.chibidevteam.semver.Version;

@Value
public class Constraint {

    private ConstraintSign   constraintSign;
    private Version          version;

    private List<Constraint> ands = new ArrayList<>();
    private List<Constraint> ors  = new ArrayList<>();

    public Constraint() {
        this.constraintSign = null;
        this.version = null;
    }

    public Constraint(ConstraintSign constraintSign, Version version) {
        this.constraintSign = constraintSign;
        this.version = version;
    }

    public void addAnd(Constraint constraint) {
        ands.add(constraint);
    }

    public void addOr(Constraint constraint) {
        ors.add(constraint);
    }

    public boolean matches(Version version) {
        if (version == null) {
            return false;
        }
        boolean matches = doesMatch(version);

        for (Constraint and : ands) {
            matches &= and.matches(version);
        }

        boolean orMatches = ors.isEmpty();
        for (Constraint or : ors) {
            orMatches |= or.matches(version);
        }

        return matches && orMatches;
    }

    private boolean doesMatch(Version v) {
        if (constraintSign == null || version == null) {
            // This is a pass through
            return true;
        }

        if (ConstraintSign.EQ.equals(constraintSign)) {
            return v.compareTo(version) == 0;
        } else if (ConstraintSign.NOT.equals(constraintSign)) {
            return v.compareTo(version) != 0;
        } else if (ConstraintSign.GT.equals(constraintSign)) {
            return v.compareTo(version) > 0;
        } else if (ConstraintSign.GTE.equals(constraintSign)) {
            return v.compareTo(version) >= 0;
        } else if (ConstraintSign.LT.equals(constraintSign)) {
            return v.compareTo(version) < 0;
        } else if (ConstraintSign.LTE.equals(constraintSign)) {
            return v.compareTo(version) <= 0;
        } else if (ConstraintSign.COMPATIBLE.equals(constraintSign)) {
            return isCompatible(v);
        } else if (ConstraintSign.NEXT_SIGNIFICANT.equals(constraintSign)) {
            return isNextSignificant(v);
        }

        // If we fall here, it means that do not handle one or more ConstraintSigns which is not acceptable.
        // In this case the library should not be released.
        return false;
    }

    private boolean isCompatible(Version v) {
        Integer partA = version.getMajor();
        Integer partB = v.getMajor();
        if (partA == 0 && partA.equals(partB)) {
            partA = version.getMinor();
            partB = v.getMinor();
        }
        return isCompOrNext(v, partA, partB);
    }

    private boolean isNextSignificant(Version v) {
        Integer partA = version.getMinor();
        Integer partB = v.getMinor();
        if (version.getPatch() == null) {
            partA = version.getMajor();
            partB = v.getMajor();
        }
        return isCompOrNext(v, partA, partB);
    }

    private boolean isCompOrNext(Version v, Integer partA, Integer partB) {
        if (v.compareTo(version) < 0) {
            return false;
        }

        if (partA == null) {
            // The only way for partB to be null here, is to try to have:
            // - NextSignificant constraint with patch and major null and version with null major
            // - Compatible constraint and version with null major
            return partB == null;
        }

        return partA.equals(partB);
    }

}
