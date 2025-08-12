package com.mechanicalorchard.jclscan.model;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public final class ProcedureRef implements JclScript {
    private String name;
}