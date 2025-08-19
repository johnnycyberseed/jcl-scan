package com.mechanicalorchard.jclscan.model;

import java.util.List;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class UnresolvedReport {
  private List<UnresolvedRecord> unresolvedItems;
}
