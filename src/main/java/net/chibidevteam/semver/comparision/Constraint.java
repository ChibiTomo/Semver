package net.chibidevteam.semver.comparision;

import java.util.ArrayList;
import java.util.List;

import lombok.EqualsAndHashCode;
import lombok.ToString;
import net.chibidevteam.semver.Version;

@ToString
@EqualsAndHashCode
public class Constraint {

    private ConstraintSign   constraintSign;
    private Version          version;

    private List<Constraint> ands = new ArrayList<>();
    private List<Constraint> ors  = new ArrayList<>();

    public Constraint() {
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
            Integer partA = v.getMajor();
            Integer partB = version.getMajor();
            if (partA == 0) {
                partA = v.getMinor();
                partB = version.getMinor();
            }
            return v.compareTo(version) >= 0 && partA == partB;
        } else if (ConstraintSign.NEXT_SIGNIFICANT.equals(constraintSign)) {
            Integer partA = v.getMajor();
            Integer partB = version.getMajor();
            if (v.getPatch() == null) {
                partA = v.getMinor();
                partB = version.getMinor();
            }
            return v.compareTo(version) >= 0 && partA == partB;
        }

        return false;
    }

}
